file_path = '/Users/heima/Desktop/Háskoli/MA_DH/WS_23/Masterseminar/Masterarbeit/Kurlisten/Kurlisten_Excel/updated_normalized_person_data(1).xlsx'
import pandas as pd
from SPARQLWrapper import SPARQLWrapper, JSON

def load_data(file_path):
    # Load the Excel data
    return pd.read_excel(file_path)

def fetch_wikidata_id_sparql(place_name):
    # SPARQL query to fetch Wikidata ID
    sparql = SPARQLWrapper("https://query.wikidata.org/sparql")
    query = f"""
    SELECT ?item WHERE {{
      ?item rdfs:label "{place_name}"@en;
            wdt:P31/wdt:P279* wd:Q486972;  # focusing on human settlements
            SERVICE wikibase:label {{ bd:serviceParam wikibase:language "en". }}
      FILTER EXISTS {{ ?item wdt:P17 ?country. ?country wdt:P30 wd:Q46. }}  # ensure it's in Europe
    }}
    LIMIT 1
    """
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    results = sparql.query().convert()
    for result in results["results"]["bindings"]:
        return result["item"]["value"].split('/')[-1]  # extracts Q-ID
    return None

# def map_wikidata_ids(file_path):
#    data = load_data(file_path)
#    unique_places = data['Unique_Place'].dropna().unique()
#    total = len(unique_places)
#    print(f"Starting to fetch Wikidata IDs for {total} unique places...")
#    for index, place in enumerate(unique_places):
#        wikidata_id = fetch_wikidata_id_sparql(place)
#        data.loc[data['Unique_Place'] == place, 'Wikidata_ID'] = wikidata_id
#        print(f"Processed {index + 1}/{total}: {place} - ID: {wikidata_id}")
#    return data

def map_wikidata_ids(file_path, start_from=0):
    data = load_data(file_path)
    unique_places = data['Unique_Place'].dropna().unique()
    total = len(unique_places)
    print(f"Starting to fetch Wikidata IDs for {total} unique places...")

    for index, place in enumerate(unique_places[start_from:], start=start_from):
        wikidata_id = fetch_wikidata_id_sparql(place)
        data.loc[data['Unique_Place'] == place, 'Wikidata_ID'] = wikidata_id
        print(f"Processed {index + 1}/{total}: {place} - ID: {wikidata_id}")
    return data

def save_data(data, file_path):
    enriched_path = file_path.replace('.xlsx', '_enriched.xlsx')
    data.to_excel(enriched_path, index=False)
    print(f"Data enriched with Wikidata IDs saved to {enriched_path}")

# def run_workflow(file_path):
#    enriched_data = map_wikidata_ids(file_path)
#    save_data(enriched_data, file_path)

def run_workflow(file_path):
    start_from = 980  # Adjust index to where you left off (981 - 1 since indexing starts at 0)
    enriched_data = map_wikidata_ids(file_path, start_from)
    save_data(enriched_data, file_path)


# Specify the file path
run_workflow(file_path)

