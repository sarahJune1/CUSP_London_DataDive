import time
import os
os.environ["PROJ_LIB"] = r'C:\Users\malvarez\AppData\Local\Continuum\miniconda3\envs\py36\Library\share'
import geopandas as gpd
import pandas as pd
import glob
import osmnx as ox
from shapely.geometry import Polygon, Point, LineString
import networkx as nx
from joblib import Parallel, delayed
from geoalchemy2 import Geometry, WKTElement
from sqlalchemy import create_engine
import psycopg2
from concurrent.futures import ThreadPoolExecutor, as_completed
from psycopg2.pool import ThreadedConnectionPool

def load_london_network():
    #check if graph already exist to load from memory
	if os.path.isfile('data/london_network.graphml'):
		print('reading network from file')
		return ox.save_load.load_graphml('london_network.graphml', folder='data')

    #if graph does not exist, download using osmnx
	else:
		print('creating network form OSM')
		place = 'London, Greater London, England, United Kingdom'
		gdf_place = ox.gdf_from_place(place, which_result=2, buffer_dist=500)
		london_polygon = gdf_place['geometry'].unary_union
        #get exterior coords and create new polygon, as city of london is a hole
		london_polygon = Polygon(london_polygon.exterior)
		london_network = ox.graph_from_polygon(london_polygon)
        #project graph to BNG
		london_network = ox.project_graph(london_network, to_crs={'init':'epsg:27700'})
        #create a data folder if it does not exist
		if not os.path.exists('data'):
			 os.makedirs('data')
        #save to file 
		ox.save_load.save_graphml(london_network, filename='london_network.graphml', folder='data', gephi=False)
		return london_network


def graph_nodes_to_db(G, schema, table_name, host, database, user, password, port = 5432):
	#check if table already in database
    conn = psycopg2.connect(host=host,database=database, user=user, password=password)
    c = conn.cursor()
    sql = "SELECT to_regclass('{}.{}');".format(schema, table_name)
    c.execute(sql)

    #if table exist do nothing
    if c.fetchall()[0][0] is not None:
        print('nodes already in database')
    else:
        #if nodes are not in database, save using geopandas
        print('saving {} to database'.format(table_name))
        nodes = gpd.GeoDataFrame(dict(G.nodes(data=True))).T
        nodes.geometry =  [Point(node['x'], node['y']) for i, node in nodes.iterrows()]

        #convert geometry to well know text format
        nodes['geom'] = nodes['geometry'].apply(lambda x: WKTElement(x.wkt, srid=27700))

        # drop the geometry column as it is now duplicative
        nodes.drop('geometry', 1, inplace=True)
        engine = create_engine('postgresql://{user}:{password}@{host}:{port}/{database}'.format(
            user = user,
            password = password, 
            host = host,
            database = database, 
            port = port
            ))

        nodes.to_sql(table_name, engine, if_exists='replace', index=False, 
                                 dtype={'geom': Geometry('POINT', srid=27700)})


def find_nearest_node(node, tcp, table_name):
    #create connection from tcp
    conn = tcp.getconn()
    c = conn.cursor()   
    #turn geometry into well know text
    point_wkt = node.wkt
    #create closest node query
    sql = "SELECT osmid FROM {} ".format(table_name) +\
          "WHERE geom && ST_expand(ST_GeomFromText('{}', 27700), 500) ".format(point_wkt) +\
          "ORDER BY ST_Distance(geom, ST_GeomFromText('{}', 27700)) ASC ".format(point_wkt) +\
          "LIMIT 1;"
    #query db
    c.execute(sql)
    result = c.fetchall()
    tcp.putconn(conn, close=True)
    
    #return result if closest node was found, otherwise return none
    try:
        v = result[0][0]
    except:
        v = None
    
    return v


def get_shortest_path(response, tcp, table_name, london_network):
    #get coordinates of dispatch and incident points
    dispatched_point = response['dispatched_location']
    incident_point = response['incident_location']
    
    #get closest node in graph
    dispatched_node = find_nearest_node(dispatched_point, tcp, table_name)
    incident_node = find_nearest_node(incident_point, tcp, table_name)

    #if no closest node was found pass
    if dispatched_node is None or incident_node is None:
        print('here')
        pass
    else:
        #get route and length of route
        route = nx.shortest_path(london_network, dispatched_node, incident_node, weight='length')
        length = nx.shortest_path_length(london_network, dispatched_node, incident_node, weight= 'length')
        #turn route into geometry
        geom = []
        for i in range(len(route)-1):
            node_i = route[i]
            node_j = route[i+1]
            edge = london_network.get_edge_data(node_i,node_j)
            if 'geometry' in edge[0].keys():
                geom.extend(list(edge[0]['geometry'].coords))
            else:
                geom.extend([(london_network.nodes[node_i]['x'], london_network.nodes[node_i]['y']),
                            (london_network.nodes[node_j]['x'], london_network.nodes[node_j]['y'])
                           ])
        #if there is not route
        if len(geom) == 0:
            pass
        else:
            #save shortest path to database
            conn = tcp.getconn()
            c = conn.cursor()
            route_geometry = LineString(geom)
            route_wkt = route_geometry.wkt
            sql = "INSERT INTO shortest_path  " +\
                  "(incidentid, activationid, distance_to_scene, network_distance, eucledian_distance, running, dohcategory, callsignkey, geom) " +\
                  "VALUES ({},{},{},{},{},{},'{}',{},ST_GeomFromText('{}', 27700))".format(response['incidentid'],
                                                                                response['activationid'],
                                                                                response['distance_to_scene'],
                                                                                length,
                                                                                response['dispatched_location'].distance(response['incident_location']),
                                                                                response['running'],
                                                                                response['dohcategory'],
                                                                                response['callsignkey'],
                                                                                route_wkt                                                      
                                                                               )

            c.execute(sql)
            conn.commit()
            tcp.putconn(conn, close=True)


def get_network_features(row, london_network, radius=700):
    start_time = time.time()
    # get subset of graph around radius of node
    G = nx.ego_graph(london_network, row['node'], radius, distance='length')

    #if there is not subset return none
    if G.size() == 0:
        print('no network')
        return None
    else:
        #calculate basic stats and return
        start_time = time.time()
        basic_stats = ox.basic_stats(G) #Calculate features
        start_time = time.time()
        extended_stats = ox.extended_stats(G,bc=True,cc=True)
        return {'id': row['id'],
                'n': basic_stats['n'],
                'm': basic_stats['m'],
                'k_avg': basic_stats['k_avg'],
                'intersection_count': basic_stats['intersection_count'],
                'edge_length_total': basic_stats['edge_length_total'],
                'edge_length_avg': basic_stats['edge_length_avg'],
                'street_length_total': basic_stats['street_length_total'],
                'street_length_avg': basic_stats['street_length_avg'],
                'street_segments_count': basic_stats['street_segments_count'],
                'circuity_avg': basic_stats['circuity_avg'],
                'self_loop_proportion': basic_stats['self_loop_proportion'],
                'avg_neighbor_degree_avg': extended_stats['avg_neighbor_degree_avg'],
                'degree_centrality_avg': extended_stats['degree_centrality_avg'],
                'clustering_coefficient_avg': extended_stats['clustering_coefficient_avg'],
                'closeness_centrality_avg': extended_stats['closeness_centrality_avg'],
                'betweenness_centrality_avg': extended_stats['betweenness_centrality_avg']}


def clip_line_poly(lines, grid, time_set = None):
    #clip lines to polygon
    if time_set != None:
        # if an hour is given subset data first and reset index
        lines = lines[lines.time_set == time_set].reset_index() 

    # Create a single polygon object for clipping
    poly = grid
    spatial_index = lines.sindex

    # Create a box for the initial intersection
    bbox = poly.bounds
    # Get a list of id's for each road line that overlaps the bounding box and subset the data to just those lines
    sidx = list(spatial_index.intersection(bbox))
    lines_sub = lines[lines.index.isin(sidx)].copy()

    # Clip the data - with these data
    lines_sub['geometry'] = lines_sub.intersection(poly)
    lines_sub = lines_sub[lines_sub.geometry.notnull()]
    # Return the clipped layer with no null geometry values
    return(lines_sub)

def weighted_average(grid, lines,i, time_set=None):
    #clip lines to grid
    hour = (time_set[0]+(time_set[1]*24))
    clipped_lines = clip_line_poly(lines, grid, time_set)
    #get sum of distances
    if len(clipped_lines) ==0:
        print('none: {}'.format(i))
        return({'index':i, 'speed':None, 'hour': hour})
    else:
        clipped_lines['percent']= clipped_lines.length / clipped_lines.length.sum()

        #weighted average
        weighted_average = (clipped_lines['speed'] * clipped_lines['percent']).sum()

        return({'index':i, 'speed':weighted_average, 'hour': hour})