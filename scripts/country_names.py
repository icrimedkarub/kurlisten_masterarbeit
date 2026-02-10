import pandas as pd
import re

# Load your Excel file
file_path = '../data/towns.xlsx'
df = pd.read_excel(file_path)

# Function to extract country code from parentheses
def extract_country(location):
    match = re.search(r'\((\w{2})\)', str(location))  # ensures it works even if the cell is empty
    return match.group(1) if match else ''

# Apply the function to create the new 'Country' column
df['Country'] = df['Location_Reference'].apply(extract_country)

# Save the updated dataframe back to Excel
output_file = 'your_file_with_country.xlsx'
df.to_excel(output_file, index=False)

print(f"Processed file saved as {output_file}")
