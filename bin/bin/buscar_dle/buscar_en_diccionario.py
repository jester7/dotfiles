import sys
import re
import requests
import html

def get_rae_definition(word):
    url = f"https://dle.rae.es/{word}"
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
    }
    
    try:
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        html_content = response.text
    except Exception as e:
        print(f"Error fetching URL: {e}", file=sys.stderr)
        return None
    
    # Regex to find the content in section.c-section within div.o-main__content
    match = re.search(r'<div class="o-main__content">.*?<section class="c-section">(.*?)</section>.*?</div>', html_content, re.DOTALL)
    if match:
        definition_html = match.group(1)
        # Remove the header
        definition_html = re.sub(r'<header class="c-section__title u-sr-only">.*?<\/header>', '', definition_html, flags=re.DOTALL)
        
        # Add a newline after div.n2.c-text-intro
        definition_html = re.sub(r'(<div class="n2 c-text-intro">.*?</div>)', r'\1\n', definition_html, flags=re.DOTALL)
        definition_text = re.sub(r"<br/?>", "\n", definition_html)
        definition_text = re.sub(r"(</(section|ol|li|h3)>)", r"\1\n", definition_text)
        definition_text = re.sub(r"<.*?>", "", definition_text)  # Remove other tags
        
        # Decode HTML entities
        definition_text = html.unescape(definition_text)
        definition_text = definition_text.strip()
        return definition_text
    else:
        return None

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Uso: python buscar_en_diccionario.py <palabra>", file=sys.stderr)
        sys.exit(1)
    
    word = sys.argv[1]
    definition = get_rae_definition(word)
    if definition:
        print(definition)
    else:
        print(f"No se encontró definición para la palabra {word}", file=sys.stderr)
        sys.exit(1)
