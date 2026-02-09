from SPARQLWrapper import SPARQLWrapper, JSON
import pandas as pd

def fetch_country_for_qid(q_id):
    """Fetch the country for a given Q-ID using the Wikidata SPARQL endpoint."""
    sparql = SPARQLWrapper("https://query.wikidata.org/sparql")
    query = f"""
    SELECT ?countryLabel WHERE {{
        wd:{q_id} wdt:P17 ?country.
        SERVICE wikibase:label {{ bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }}
    }}
    """
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    try:
        results = sparql.query().convert()
        if results["results"]["bindings"]:
            return results["results"]["bindings"][0]["countryLabel"]["value"]
        else:
            return "No country found"
    except Exception as e:
        print(f"Failed to fetch country for {q_id}: {e}")
        return None

def update_countries(file_path):
    """Update the DataFrame with countries based on Q-IDs."""
    data = pd.read_excel(file_path)

    # Check the Q-ID column name and adjust if necessary
    qid_column = 'Wikidata_ID'  # Make sure this matches your Excel file's column for Wikidata IDs
    if qid_column not in data.columns:
        print(f"The column '{qid_column}' does not exist in the data.")
        return

    # Process each Q-ID to fetch the country
    print("Starting to fetch countries based on Q-IDs...")
    data['Country'] = data[qid_column].dropna().apply(fetch_country_for_qid)

    # Save the updated DataFrame
    updated_file_path = file_path.replace('.xlsx', '_with_countries.xlsx')
    data.to_excel(updated_file_path, index=False)
    print(f"Updated Excel file saved to {updated_file_path}.")

# Specify the path to your Excel file
file_path = '../data/spadata.xlsx'
update_countries(file_path)
