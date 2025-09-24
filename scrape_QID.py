import pandas as pd
from SPARQLWrapper import SPARQLWrapper, JSON

file_path = '/Users/heima/Desktop/Masterarbeit/Kurlisten/Kurlisten_Excel/kurdata.xlsx'

def load_data(file_path):
    # Load the Excel data
    return pd.read_excel(file_path)

def fetch_wikidata_id_sparql(place_name):
    # SPARQL query to fetch Wikidata ID
    sparql = SPARQLWrapper("https://query.wikidata.org/sparql")
    query = f"""
    SELECT ?item WHERE {{
      ?item rdfs:label "{place_name}"@en;
            wdt:P31/wdt:P279* wd:Q486972;  # human settlements
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

def map_wikidata_ids(file_path):
    data = load_data(file_path)
    unique_places = data['Unique_Place'].dropna().unique()
    total = len(unique_places)
    print(f"Starting to fetch Wikidata IDs for {total} unique places...")

    for index, place in enumerate(unique_places):
        # Skip if Wikidata_ID is already filled
        if pd.notna(data.loc[data['Unique_Place'] == place, 'Wikidata_ID']).any():
            print(f"Skipping {place} (already has Wikidata_ID)")
            continue

        wikidata_id = fetch_wikidata_id_sparql(place)
        data.loc[data['Unique_Place'] == place, 'Wikidata_ID'] = wikidata_id
        print(f"Processed {index + 1}/{total}: {place} - ID: {wikidata_id}")

    return data

def save_data(data, file_path):
    enriched_path = file_path.replace('.xlsx', '_enriched.xlsx')
    data.to_excel(enriched_path, index=False)
    print(f"Data enriched with Wikidata IDs saved to {enriched_path}")

def run_workflow(file_path):
    enriched_data = map_wikidata_ids(file_path)
    save_data(enriched_data, file_path)

# Run the workflow
run_workflow(file_path)