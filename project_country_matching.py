#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Microbiome Project Country Extraction Script
Uses online APIs to extract and normalize country information from project metadata.
"""

import pandas as pd
import pycountry
import requests
import re
import time
import xml.etree.ElementTree as ET
import json
from tqdm import tqdm
from functools import lru_cache

# -----------------------------
# CONFIGURATION - Adjust these
# -----------------------------
METADATA_FILE = 'projects.txt'  # Change to your actual file path
API_KEY = 'API_KEY'  # Your NCBI API key here

# Column names from your metadata file
PROJECT_ID_COL = 'Project_ID'
POPULATION_COL = 'Population'
CURATED_DESC_COL = 'Curated_Project_description'
ORIGINAL_DESC_COL = 'Original_Project_description'
PUBLICATIONS_COL = 'Related_Publications'

# Feature flags
ENABLE_CITY_LOOKUP = True   # Now safe to enable with context requirements
                            # City lookup now requires cities to be mentioned
                            # alongside their country (e.g., "Trento, Italy")
                            # to prevent false positives

# Cache for API lookups (reduces redundant calls)
CACHE_FILE = 'country_lookup_cache.json'

# Manual institution-to-country mapping for common institutions
# This is checked BEFORE calling ROR API for faster lookups
INSTITUTION_COUNTRY_MAP = {
    # Japanese institutions
    'Osaka University': 'Japan',
    'University of Tokyo': 'Japan',
    'Tokyo University': 'Japan',
    'Kyoto University': 'Japan',
    'RIKEN': 'Japan',
    'Keio University': 'Japan',
    'Tohoku University': 'Japan',
    
    # Chinese institutions
    'BGI': 'China',
    'BGI-Shenzhen': 'China',
    'BGI-Qingdao': 'China',
    'Chinese Academy of Sciences': 'China',
    'Beijing Genomics Institute': 'China',
    'Peking University': 'China',
    'Tsinghua University': 'China',
    'Fudan University': 'China',
    
    # South Korean institutions
    'Seoul National University': 'South Korea',
    'Yonsei University': 'South Korea',
    'KAIST': 'South Korea',
    
    # Indian institutions
    'Indian Institute of Science': 'India',
    'CSIR': 'India',
    'IIT': 'India',
    'NCBS': 'India',
    
    # European institutions
    'EMBL-EBI': 'United Kingdom',
    'Wellcome Trust Sanger Institute': 'United Kingdom',
    'Wellcome Sanger Institute': 'United Kingdom',
    'Sanger Institute': 'United Kingdom',
    'University of Cambridge': 'United Kingdom',
    'University of Oxford': 'United Kingdom',
    'Imperial College London': 'United Kingdom',
    'Max Planck Institute': 'Germany',
    'EMBL': 'Germany',
    'Helmholtz': 'Germany',
    'Karolinska Institute': 'Sweden',
    'Karolinska Institutet': 'Sweden',
    'ETH Zurich': 'Switzerland',
    'Institut Pasteur': 'France',
    
    # US institutions
    'Broad Institute': 'United States',
    'NIH': 'United States',
    'National Institutes of Health': 'United States',
    'CDC': 'United States',
    'Stanford University': 'United States',
    'Harvard University': 'United States',
    'MIT': 'United States',
    'Massachusetts Institute of Technology': 'United States',
    'Washington University': 'United States',
    'University of California': 'United States',
    'UC Berkeley': 'United States',
    'UCSD': 'United States',
    'Yale University': 'United States',
    'Johns Hopkins University': 'United States',
    'Columbia University': 'United States',
    
    # Australian institutions
    'University of Queensland': 'Australia',
    'University of Melbourne': 'Australia',
    'CSIRO': 'Australia',
    
    # Add more as needed based on your data
}

# Early-life keywords pattern
EARLY_LIFE_PATTERN = re.compile(
    r'\b(?:infant|infants|neonatal|neonate|newborn|baby|babies|child|children|toddler|toddlers|'
    r'pediatric|paediatric|under 5|<5 years|0-5 years|early life|early-life|perinatal|'
    r'postnatal early|under five|maternal|pregnancy|pregnant|birth cohort)\b',
    re.IGNORECASE
)

# -----------------------------
# CACHING SYSTEM
# -----------------------------

class CountryCache:
    """Cache for country lookups to avoid repeated API calls."""
    
    def __init__(self, cache_file=CACHE_FILE):
        self.cache_file = cache_file
        self.cache = self._load_cache()
    
    def _load_cache(self):
        try:
            with open(self.cache_file, 'r') as f:
                return json.load(f)
        except FileNotFoundError:
            return {
                'city_to_country': {},
                'place_to_country': {},
                'institution_to_country': {}
            }
    
    def save(self):
        with open(self.cache_file, 'w') as f:
            json.dump(self.cache, f, indent=2)
    
    def get_city(self, city):
        return self.cache['city_to_country'].get(city)
    
    def set_city(self, city, country):
        self.cache['city_to_country'][city] = country
        self.save()
    
    def get_place(self, place):
        return self.cache['place_to_country'].get(place)
    
    def set_place(self, place, country):
        self.cache['place_to_country'][place] = country
        self.save()
    
    def get_institution(self, institution):
        return self.cache['institution_to_country'].get(institution)
    
    def set_institution(self, institution, country):
        self.cache['institution_to_country'][institution] = country
        self.save()


# Initialize global cache
country_cache = CountryCache()


# -----------------------------
# ONLINE COUNTRY DATABASES
# -----------------------------

@lru_cache(maxsize=1000)
def get_all_country_names():
    """
    Get comprehensive list of country names from pycountry.
    Returns dict mapping all variations to canonical names.
    """
    country_map = {}
    
    for country in pycountry.countries:
        # Official name
        country_map[country.name] = country.name
        
        # Common name (if different)
        if hasattr(country, 'common_name'):
            country_map[country.common_name] = country.name
        
        # Official name variants
        if hasattr(country, 'official_name'):
            country_map[country.official_name] = country.name
    
    # Add manual aliases for cases pycountry fuzzy matching cannot handle
    # Only include: abbreviations, informal names, and ambiguous cases
    manual_aliases = {
        # Abbreviations (fuzzy match fails on short strings)
        'USA': 'United States',
        'US': 'United States',
        'U.S.': 'United States',
        'U.S.A.': 'United States',
        'UK': 'United Kingdom',
        'U.K.': 'United Kingdom',
        'DRC': 'Congo, The Democratic Republic of the',
        
        # Informal/common names (fuzzy match unreliable)
        'America': 'United States',
        'Britain': 'United Kingdom',
        'Great Britain': 'United Kingdom',
        'Holland': 'Netherlands',
        
        # UK regions (commonly mentioned separately in text)
        'England': 'United Kingdom',
        'Scotland': 'United Kingdom',
        'Wales': 'United Kingdom',
        'Northern Ireland': 'United Kingdom',
        
        # Ambiguous cases requiring explicit mapping
        'Congo': 'Congo',  # Defaults to Republic of Congo (not DRC)
        'Taiwan': 'Taiwan, Province of China',
        'Korea': 'Korea, Republic of',  # Default "Korea" to South Korea (North Korean projects highly unlikely)
        'South Korea': 'Korea, Republic of',
        'North Korea': "Korea, Democratic People's Republic of",  # Only if explicitly mentioned
        
        # Special non-English names (ASCII versions)
        'Eire': 'Ireland',  # Irish name without accent
        'Ayiti': 'Haiti',   # Haitian Creole name
    }
    # Note: Most country name variations (Brasil, Deutschland, Espana, etc.)
    # are handled automatically by pycountry's fuzzy matching
    
    country_map.update(manual_aliases)
    
    return country_map


# Optional: Import thefuzz for fuzzy institution matching (improves accuracy)
try:
    from thefuzz import process
    FUZZY_MATCHING_AVAILABLE = True
except ImportError:
    FUZZY_MATCHING_AVAILABLE = False
    # Will fall back to exact matching only


def find_best_institution_match(query_name, institution_map, threshold=85):
    """
    Find best matching institution using fuzzy matching if available.
    Falls back to exact matching if thefuzz not installed.
    
    Args:
        query_name: Institution name to match
        institution_map: Dictionary of known institutions
        threshold: Minimum similarity score (0-100)
    
    Returns:
        Country name if match found, None otherwise
    """
    # First: Try exact match (fastest)
    if query_name in institution_map:
        return institution_map[query_name]
    
    # Second: Try case-insensitive exact match
    query_lower = query_name.lower()
    for inst, country in institution_map.items():
        if inst.lower() == query_lower:
            return country
    
    # Third: Try fuzzy matching if available
    if FUZZY_MATCHING_AVAILABLE:
        best_match = process.extractOne(
            query_name,
            institution_map.keys(),
            score_cutoff=threshold
        )
        
        if best_match:
            matched_name, score = best_match[0], best_match[1]
            # Log fuzzy matches for transparency (accuracy priority)
            if score < 100:  # Not exact match
                print(f"  Fuzzy matched: '{query_name}' ? '{matched_name}' (similarity: {score}%)")
            return institution_map[matched_name]
    
    return None


@lru_cache(maxsize=500)
def get_demonym_to_country(demonym):
    """
    Use REST Countries API to get country from demonym.
    Free, no API key required: https://restcountries.com/
    Returns English country name only.
    """
    # Check cache first
    cached = country_cache.get_place(f"demonym:{demonym}")
    if cached:
        return cached
    
    try:
        url = f"https://restcountries.com/v3.1/demonym/{demonym}"
        response = requests.get(url, timeout=5)
        
        if response.status_code == 200:
            data = response.json()
            if data and len(data) > 0:
                # Get common name in English
                country_data = data[0].get('name', {})
                country_name = country_data.get('common')  # This is in English
                
                if country_name:
                    # Normalize to pycountry standard (ensures English)
                    country_name = normalize_country_name(country_name)
                    # Only cache and return if it's ASCII (English)
                    if country_name and country_name.isascii():
                        country_cache.set_place(f"demonym:{demonym}", country_name)
                        return country_name
    except Exception as e:
        print(f"  Error fetching demonym '{demonym}': {e}")
    
    return None


def lookup_city_country(city_name):
    """
    Use OpenStreetMap Nominatim API to get country from city.
    Free, but requires respectful usage: https://nominatim.org/
    Returns English country name only.
    """
    # Check cache first
    cached = country_cache.get_city(city_name)
    if cached:
        return cached
    
    try:
        url = "https://nominatim.openstreetmap.org/search"
        params = {
            'city': city_name,
            'format': 'json',
            'limit': 1,
            'accept-language': 'en'  # Request English results
        }
        headers = {
            'User-Agent': 'MicrobiomeCountryExtractor/1.0'  # Required by Nominatim
        }
        
        response = requests.get(url, params=params, headers=headers, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            if data and len(data) > 0:
                # Try to get country from address field (more reliable)
                address = data[0].get('address', {})
                potential_country = address.get('country')
                
                # Fallback: get from display_name
                if not potential_country:
                    display_name = data[0].get('display_name', '')
                    parts = [p.strip() for p in display_name.split(',')]
                    if parts:
                        potential_country = parts[-1]
                
                if potential_country:
                    # Normalize to ensure English name
                    country_name = normalize_country_name(potential_country)
                    # Only cache and return if it's an English ASCII name
                    if country_name and country_name.isascii():
                        country_cache.set_city(city_name, country_name)
                        return country_name
        
        time.sleep(1)  # Nominatim requires 1 request per second
        
    except Exception as e:
        print(f"  Error looking up city '{city_name}': {e}")
    
    return None


def lookup_institution_country(institution_name):
    """
    Use ROR (Research Organization Registry) API to get institution country.
    Free, no API key required: https://ror.org/
    Returns English country name only.
    """
    # Check cache first
    cached = country_cache.get_institution(institution_name)
    if cached:
        return cached
    
    try:
        url = "https://api.ror.org/organizations"
        params = {"query": institution_name}
        
        response = requests.get(url, params=params, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            if data.get('number_of_results', 0) > 0:
                # Get top result
                top_result = data['items'][0]
                country_info = top_result.get('country', {})
                country_name = country_info.get('country_name')  # This is in English
                
                if country_name:
                    country_name = normalize_country_name(country_name)
                    # Only cache and return if it's ASCII (English)
                    if country_name and country_name.isascii():
                        country_cache.set_institution(institution_name, country_name)
                        return country_name
        
        time.sleep(0.5)  # Be respectful to ROR
        
    except Exception as e:
        print(f"  Error looking up institution '{institution_name}': {e}")
    
    return None


def fetch_ena_center_name(project_id):
    """
    Fetch center name from ENA with multiple fallback strategies.
    Prioritizes accuracy over speed - tries XML API then Portal API.
    """
    if not project_id or pd.isna(project_id):
        return None
    
    # Check cache first
    cache_key = f"ena_center:{project_id}"
    cached = country_cache.get_place(cache_key)
    if cached:
        return cached
    
    center = None
    
    # STRATEGY 1: Try XML API (has more detailed metadata)
    url_xml = f"https://www.ebi.ac.uk/ena/browser/api/xml/{project_id}"
    
    try:
        response = requests.get(url_xml, timeout=15)
        
        if response.status_code == 200:
            root = ET.fromstring(response.content)
            
            # Try multiple XPath patterns (ENA has inconsistent formats)
            patterns = [
                './/CENTER_NAME',
                './/DESCRIPTOR/CENTER_NAME',
                './/STUDY/DESCRIPTOR/CENTER_NAME',
                './/*[local-name()="CENTER_NAME"]',
            ]
            
            for pattern in patterns:
                center_elem = root.find(pattern)
                if center_elem is not None and center_elem.text:
                    center = center_elem.text.strip()
                    if center:  # Make sure it's not empty
                        break
            
            if center:
                country_cache.set_place(cache_key, center)
                time.sleep(0.3)  # Respectful rate limiting
                return center
    
    except ET.ParseError:
        pass  # XML parsing failed, try portal API
    except requests.Timeout:
        pass  # Timeout, try portal API
    except Exception:
        pass  # Any other error, try portal API
    
    # STRATEGY 2: Try Portal API (JSON format - often more reliable)
    url_portal = "https://www.ebi.ac.uk/ena/portal/api/search"
    params = {
        'result': 'study',
        'query': f'study_accession={project_id}',
        'fields': 'center_name',
        'format': 'json',
        'limit': 1
    }
    
    try:
        response = requests.get(url_portal, params=params, timeout=15)
        
        if response.status_code == 200:
            data = response.json()
            if data and len(data) > 0:
                center = data[0].get('center_name', '')
                if center and center.strip() and center.strip().lower() != 'null':
                    center = center.strip()
                    country_cache.set_place(cache_key, center)
                    time.sleep(0.3)
                    return center
    
    except Exception:
        pass  # Portal API also failed
    
    # Both strategies failed
    time.sleep(0.3)  # Rate limiting even on failure
    return None


# -----------------------------
# NORMALIZATION FUNCTIONS
# -----------------------------

def normalize_country_name(country_str):
    """
    Normalize country name to standard pycountry format (English only).
    Uses fuzzy matching if exact match not found.
    """
    if not country_str or pd.isna(country_str):
        return None
    
    # Convert to string and strip whitespace
    try:
        country_str = str(country_str).strip()
    except:
        return None
    
    if not country_str or country_str.lower() in ['unknown', 'nan', '']:
        return None
    
    # Get all country mappings
    country_map = get_all_country_names()
    
    # Try exact match (case-insensitive) - only ASCII names
    for key, canonical in country_map.items():
        if key.isascii() and country_str.lower() == key.lower():
            return canonical
    
    # Try fuzzy match using pycountry (only returns English names)
    try:
        result = pycountry.countries.search_fuzzy(country_str)
        if result:
            # Always return the English 'name' field, not local names
            return result[0].name
    except LookupError:
        pass
    except Exception:
        # Skip any encoding or other errors
        pass
    
    # If it contains non-ASCII, try to handle common patterns
    if not country_str.isascii():
        # Check if it might be a accented version of a known country
        # by trying to match against pycountry with the non-ASCII string
        try:
            # Remove common accents and try again
            import unicodedata
            # Normalize to NFD (decompose) then remove combining characters
            normalized = unicodedata.normalize('NFD', country_str)
            ascii_version = ''.join(c for c in normalized if unicodedata.category(c) != 'Mn')
            
            # Try the ASCII version
            for key, canonical in country_map.items():
                if key.isascii() and ascii_version.lower() == key.lower():
                    return canonical
        except:
            pass
        
        # Still non-ASCII, reject it
        return None
    
    # Return original only if it's ASCII
    return country_str if country_str.isascii() else None


# -----------------------------
# TEXT EXTRACTION FUNCTIONS
# -----------------------------

def extract_countries_from_text(text):
    """
    Extract countries from text using pattern matching and online lookups.
    Returns list of (country, confidence) tuples.
    """
    if pd.isna(text) or not text:
        return []
    
    text_str = str(text)
    results = []
    
    # Get all possible country names for pattern matching
    country_map = get_all_country_names()
    all_country_terms = list(country_map.keys())
    
    # Sort by length (longest first) to match "United States" before "States"
    all_country_terms.sort(key=len, reverse=True)
    
    # 1. Strong context patterns - STRICT VERSION (reduces false positives)
    # Requires location mention immediately after context words (no distant references)
    strong_pattern = re.compile(
        # Optional possessive/demonstrative
        r'(?:our|the|these|this)?\s*'
        # Subject (what we're collecting/studying)
        r'(?:samples?|participants?|subjects?|cohort|patients?|children|infants|'
        r'individuals|study|data)\b\s+'
        # Location context (must be adjacent - max 3 words between)
        r'(?:\w+\s+){0,3}'  # Allow up to 3 words between (e.g., "were collected from")
        r'(?:from|in|collected in|recruited in|recruited from|originating from|'
        r'residing in|living in|enrolled in|obtained from)\s+'
        # Location capture (country/city name)
        r'([A-Z][a-zA-Z\s]+?)'
        # Terminators
        r'(?:\.|,|;|$|\s+(?:were|was|and|or|with)\b)',
        re.IGNORECASE
    )
    
    for match in strong_pattern.finditer(text_str):
        location = match.group(1).strip()
        
        # Try to normalize as country
        country = normalize_country_name(location)
        if country and country in [v for v in country_map.values()]:
            results.append((country, 'strong_context'))
            continue
        
        # Note: City lookup WITHOUT country context removed
        # (causes too many false positives)
    
    # 2. Context-aware city lookup (SAFE - requires city + country together)
    # Pattern: "City, Country" or "City in Country" or "University of City, Country"
    if ENABLE_CITY_LOOKUP:
        city_country_pattern = re.compile(
            r'(?:from|in|at|hospital in|hospital of|university of|institute in|institute of|'
            r'clinic in|clinic of|center in|center of|centre in|centre of|located in)\s+'
            r'([A-Z][a-zA-Z\s]+?)\s*,\s*'
            r'(' + '|'.join(re.escape(c) for c in all_country_terms if len(c) >= 4) + r')\b',
            re.IGNORECASE
        )
        
        for match in city_country_pattern.finditer(text_str):
            city_name = match.group(1).strip()
            country_name = match.group(2).strip()
            
            # Normalize the country name
            country = normalize_country_name(country_name)
            if country:
                results.append((country, 'city_with_country'))
                # Note: We already have the country, no need to look up the city
    
    # 3. Check for demonyms with strong context
    # Use REST Countries API to validate demonyms dynamically
    potential_demonyms = re.findall(
        r'\b([A-Z][a-z]+(?:ese|ian|ish|i|an))\b\s+(?:cohort|participants?|subjects?|samples?|'
        r'patients?|children|infants|population)',
        text_str
    )
    
    for demonym in set(potential_demonyms):
        country = get_demonym_to_country(demonym)
        if country:
            results.append((country, 'demonym'))
            time.sleep(0.5)
    
    # 4. Direct country name mentions
    for country_term in all_country_terms:
        if len(country_term) < 4:  # Skip very short terms to avoid false positives
            continue
        
        pattern = r'\b' + re.escape(country_term) + r'\b'
        if re.search(pattern, text_str, re.IGNORECASE):
            canonical = country_map[country_term]
            results.append((canonical, 'direct_mention'))
    
    # Deduplicate and prioritize by confidence
    seen = {}
    for country, confidence in results:
        if country not in seen:
            seen[country] = confidence
        else:
            # Keep highest confidence
            confidence_order = ['strong_context', 'city_with_country', 'demonym', 'direct_mention']
            current_idx = confidence_order.index(seen[country]) if seen[country] in confidence_order else 999
            new_idx = confidence_order.index(confidence) if confidence in confidence_order else 999
            if new_idx < current_idx:
                seen[country] = confidence
    
    return [(c, conf) for c, conf in seen.items()]


def format_country_assignment(country_list):
    """
    Format list of (country, confidence) tuples into assignment string.
    """
    if not country_list:
        return None
    
    countries = [c for c, _ in country_list]
    confidences = [conf for _, conf in country_list]
    
    if len(countries) == 1:
        return f"{countries[0]} ({confidences[0]})"
    else:
        return f"Multi-country ({confidences[0]}): {'; '.join(sorted(set(countries)))}"


# -----------------------------
# PUBMED FUNCTIONS
# -----------------------------

def fetch_abstract(pmid):
    """Fetch abstract from PubMed for a given PMID."""
    if pd.isna(pmid) or not str(pmid).strip():
        return None
    
    try:
        pmid = str(int(float(pmid)))
    except:
        return None
    
    url = f"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id={pmid}&retmode=xml"
    if API_KEY:
        url += f"&api_key={API_KEY}"
    
    try:
        response = requests.get(url, timeout=10)
        if response.status_code == 200:
            root = ET.fromstring(response.content)
            abstract_texts = root.findall(".//AbstractText")
            if abstract_texts:
                return ' '.join([elem.text or '' for elem in abstract_texts])
            abstract = root.find(".//Abstract")
            if abstract is not None and abstract.text:
                return abstract.text
    except Exception as e:
        print(f"  Error fetching abstract PMID {pmid}: {e}")
    
    return None


def fetch_affiliation_country(pmid):
    """Extract country from last author affiliation."""
    try:
        pmid = str(int(float(pmid)))
    except:
        return None
    
    url = f"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id={pmid}&retmode=xml"
    if API_KEY:
        url += f"&api_key={API_KEY}"
    
    try:
        response = requests.get(url, timeout=10)
        if response.status_code == 200:
            root = ET.fromstring(response.content)
            affiliations = root.findall(".//AffiliationInfo/Affiliation")
            if affiliations:
                last_aff = affiliations[-1].text or ''
                country_list = extract_countries_from_text(last_aff)
                return format_country_assignment(country_list)
    except Exception as e:
        print(f"  Error fetching affiliation PMID {pmid}: {e}")
    
    return None


def get_text_from_element(elem):
    """Recursively extract text from XML element."""
    text = elem.text or ''
    for child in elem:
        text += get_text_from_element(child)
    text += elem.tail or ''
    return text.strip()


def fetch_methods_text(pmid):
    """Fetch methods section from PMC if available."""
    try:
        pmid = str(int(float(pmid)))
    except:
        return None
    
    # First, get PMC ID
    elink_url = f"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&db=pmc&id={pmid}&retmode=json"
    if API_KEY:
        elink_url += f"&api_key={API_KEY}"
    
    try:
        response = requests.get(elink_url, timeout=10)
        if response.status_code != 200:
            return None
        
        data = response.json()
        linksets = data.get('linksets', [])
        if not linksets or 'linksetdbs' not in linksets[0]:
            return None
        
        pmcid = None
        for link in linksets[0]['linksetdbs']:
            if link['dbto'] == 'pmc':
                pmc_links = link.get('links', [])
                if pmc_links:
                    pmcid = 'PMC' + str(pmc_links[0])
                    break
        
        if not pmcid:
            return None
    except Exception as e:
        return None
    
    # Fetch full text from PMC
    efetch_url = f"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pmc&id={pmcid}&retmode=xml"
    if API_KEY:
        efetch_url += f"&api_key={API_KEY}"
    
    try:
        response = requests.get(efetch_url, timeout=15)
        if response.status_code != 200:
            return None
        
        root = ET.fromstring(response.content)
        methods_text = ''
        
        # Look for methods sections
        for sec in root.findall('.//sec'):
            sec_type = sec.get('sec-type', '').lower()
            title_elem = sec.find('.//title')
            title = title_elem.text.lower() if title_elem is not None and title_elem.text else ''
            
            if 'method' in sec_type or 'method' in title or 'materials' in sec_type or 'materials' in title:
                methods_text += ' ' + get_text_from_element(sec)
        
        return methods_text.strip() if methods_text.strip() else None
    
    except Exception as e:
        return None


# Institution detection pattern
inst_pattern = re.compile(
    r'(University of [A-Z]\w+|'
    r'[A-Z][a-z]+ University|'
    r'[A-Z][a-z]+ Institute|'
    r'National Institute|'
    r'[A-Z][a-z]+ College|'
    r'Institute of [A-Z]\w+)',
    re.IGNORECASE
)


# -----------------------------
# PARSING FUNCTIONS
# -----------------------------

def parse_and_normalize_countries(assigned):
    """
    Parse assigned country string and normalize all country names.
    """
    if pd.isna(assigned) or assigned in ['Unknown', '']:
        return ['Unknown']
    
    # Strip confidence tags
    base_str = re.sub(r'\s*\([^)]+\)$', '', assigned).strip()
    
    if base_str.startswith('Multi-country'):
        parts = base_str.split(':', 1)
        if len(parts) > 1:
            countries_str = parts[1].strip()
            countries = [c.strip() for c in countries_str.split(';') if c.strip()]
        else:
            countries = []
    else:
        countries = [base_str] if base_str else []
    
    # Normalize each country
    normalized = []
    for c in countries:
        c_norm = normalize_country_name(c)
        if c_norm and c_norm != 'Unknown':
            normalized.append(c_norm)
    
    return normalized if normalized else ['Unknown']


def print_progress_summary(df, step_name):
    """Print a summary of country assignment progress."""
    total = len(df)
    
    # Check which columns exist
    assigned_mask = pd.Series([False] * total, index=df.index)
    
    if 'country_from_population' in df.columns:
        assigned_mask |= df['country_from_population'].notna()
    if 'country_from_text' in df.columns:
        assigned_mask |= df['country_from_text'].notna()
    if 'country_from_abstract' in df.columns:
        assigned_mask |= df['country_from_abstract'].notna()
    if 'country_from_fulltext' in df.columns:
        assigned_mask |= df['country_from_fulltext'].notna()
    if 'country_from_affiliation' in df.columns:
        assigned_mask |= df['country_from_affiliation'].notna()
    if 'country_from_institution' in df.columns:
        assigned_mask |= df['country_from_institution'].notna()
    
    assigned = assigned_mask.sum()
    remaining = total - assigned
    pct_complete = (assigned / total * 100) if total > 0 else 0
    
    print(f"\n{'-'*70}")
    print(f"Progress after {step_name}:")
    print(f"  Assigned: {assigned}/{total} ({pct_complete:.1f}%)")
    print(f"  Remaining: {remaining}")
    print(f"{'-'*70}")



# -----------------------------
# MAIN PROCESSING
# -----------------------------

def main():
    print("="*70)
    print("MICROBIOME PROJECT COUNTRY EXTRACTION")
    print("Using Online APIs for Country/City/Institution Lookup")
    if ENABLE_CITY_LOOKUP:
        print("NOTE: City lookup requires 'City, Country' format (safe mode)")
    print("="*70)
    print(f"\nLoading metadata from: {METADATA_FILE}")
    
    df = pd.read_csv(METADATA_FILE, sep='\t', quotechar='"', low_memory=False)
    print(f"Loaded {len(df)} projects.")
    
    # Step 1: Population column
    print("\n" + "="*70)
    print("STEP 1: Population Column")
    print("="*70)
    df['country_from_population'] = df[POPULATION_COL].apply(
        lambda x: normalize_country_name(x) if pd.notna(x) and str(x).strip() else None
    )
    pop_assigned = df['country_from_population'].notna().sum()
    print(f"[OK] Assigned {pop_assigned} projects from Population column")
    print_progress_summary(df, "Population column")
    
    # Step 2: Descriptions
    print("\n" + "="*70)
    print("STEP 2: Project Descriptions")
    print("="*70)
    df['text_for_search'] = df[CURATED_DESC_COL].fillna('') + ' ' + df[ORIGINAL_DESC_COL].fillna('')
    
    def extract_from_description(text):
        country_list = extract_countries_from_text(text)
        return format_country_assignment(country_list)
    
    print("Extracting countries (this may take a while due to API lookups)...")
    tqdm.pandas(desc="Descriptions")
    df['country_from_text'] = df['text_for_search'].progress_apply(extract_from_description)
    text_assigned = df['country_from_text'].notna().sum()
    print(f"[OK] Extracted country info for {text_assigned} projects")
    print_progress_summary(df, "Project descriptions")
    
    # Early-life detection
    print("\nDetecting early-life projects...")
    tqdm.pandas(desc="Early-life scan")
    df['is_early_life'] = df['text_for_search'].progress_apply(
        lambda x: 1 if re.search(EARLY_LIFE_PATTERN, str(x)) else 0
    )
    early_life_count = df['is_early_life'].sum()
    print(f"[OK] Detected {early_life_count} early-life projects")
    
    # Step 3: PubMed abstracts
    print("\n" + "="*70)
    print("STEP 3: PubMed Abstracts")
    print("="*70)
    df['country_from_abstract'] = None
    missing_after_text = df[df['country_from_population'].isna() & df['country_from_text'].isna()]
    
    print(f"Fetching abstracts for {len(missing_after_text)} projects...")
    abstract_count = 0
    
    with tqdm(total=len(missing_after_text), desc="Abstracts") as pbar:
        for idx, row in missing_after_text.iterrows():
            pmids_str = row.get(PUBLICATIONS_COL)
            if pd.isna(pmids_str):
                pbar.update(1)
                continue
            
            pmids = [p.strip() for p in str(pmids_str).split(',') if p.strip()]
            pmids = [p for p in pmids if p.replace('.', '').isdigit()]
            
            full_abstract_text = ''
            for pmid in pmids[:3]:  # Limit to first 3 PMIDs
                abstract = fetch_abstract(pmid)
                if abstract:
                    full_abstract_text += ' ' + abstract
                time.sleep(0.1 if API_KEY else 0.35)
            
            if full_abstract_text:
                country_list = extract_countries_from_text(full_abstract_text)
                country = format_country_assignment(country_list)
                if country:
                    df.at[idx, 'country_from_abstract'] = country
                    abstract_count += 1
                    pbar.set_postfix({'assigned': abstract_count})
                
                if re.search(EARLY_LIFE_PATTERN, full_abstract_text):
                    df.at[idx, 'is_early_life'] = 1
            
            pbar.update(1)
    
    print(f"[OK] Assigned {abstract_count} projects from abstracts")
    print_progress_summary(df, "PubMed abstracts")
    
    # Step 4: Full-text methods
    print("\n" + "="*70)
    print("STEP 4: Full-Text Methods (PMC)")
    print("="*70)
    df['country_from_fulltext'] = None
    missing_after_abstract = df[
        df['country_from_population'].isna() &
        df['country_from_text'].isna() &
        df['country_from_abstract'].isna()
    ]
    
    print(f"Attempting full-text extraction for {len(missing_after_abstract)} projects...")
    fulltext_count = 0
    
    with tqdm(total=len(missing_after_abstract), desc="Full-text") as pbar:
        for idx, row in missing_after_abstract.iterrows():
            pmids_str = row.get(PUBLICATIONS_COL)
            if pd.isna(pmids_str):
                pbar.update(1)
                continue
            
            pmids = [p.strip() for p in str(pmids_str).split(',') if p.strip()]
            pmids = [p for p in pmids if p.replace('.', '').isdigit()]
            
            found = False
            for pmid in pmids[:3]:
                methods_text = fetch_methods_text(pmid)
                if methods_text:
                    country_list = extract_countries_from_text(methods_text)
                    country = format_country_assignment(country_list)
                    if country:
                        df.at[idx, 'country_from_fulltext'] = country
                        fulltext_count += 1
                        pbar.set_postfix({'assigned': fulltext_count})
                        found = True
                    
                    if re.search(EARLY_LIFE_PATTERN, methods_text):
                        df.at[idx, 'is_early_life'] = 1
                    
                    if found:
                        break
                
                time.sleep(0.1 if API_KEY else 0.7)
            
            pbar.update(1)
    
    print(f"[OK] Assigned {fulltext_count} projects from full-text")
    print_progress_summary(df, "Full-text methods")
    
    # Step 5: Affiliations
    print("\n" + "="*70)
    print("STEP 5: Author Affiliations")
    print("="*70)
    df['country_from_affiliation'] = None
    missing_after_fulltext = df[
        df['country_from_population'].isna() &
        df['country_from_text'].isna() &
        df['country_from_abstract'].isna() &
        df['country_from_fulltext'].isna()
    ]
    
    print(f"Extracting affiliation countries for {len(missing_after_fulltext)} projects...")
    affiliation_count = 0
    
    with tqdm(total=len(missing_after_fulltext), desc="Affiliations") as pbar:
        for idx, row in missing_after_fulltext.iterrows():
            pmids_str = row.get(PUBLICATIONS_COL)
            if pd.isna(pmids_str):
                pbar.update(1)
                continue
            
            pmids = [p.strip() for p in str(pmids_str).split(',') if p.strip()]
            pmids = [p for p in pmids if p.replace('.', '').isdigit()]
            
            found = False
            for pmid in pmids[:3]:
                country = fetch_affiliation_country(pmid)
                if country:
                    df.at[idx, 'country_from_affiliation'] = country
                    affiliation_count += 1
                    pbar.set_postfix({'assigned': affiliation_count})
                    found = True
                    break
                
                time.sleep(0.1 if API_KEY else 0.35)
            
            pbar.update(1)
    
    print(f"[OK] Assigned {affiliation_count} projects from affiliations")
    print_progress_summary(df, "Author affiliations")
    
    # Step 6: Institutions via ENA + ROR
    print("\n" + "="*70)
    print("STEP 6: Institution Lookup (ENA + ROR)")
    print("="*70)
    df['country_from_institution'] = None
    df['ena_center_name'] = None  # Store center names for reference
    
    missing_final = df[
        df['country_from_population'].isna() &
        df['country_from_text'].isna() &
        df['country_from_abstract'].isna() &
        df['country_from_fulltext'].isna() &
        df['country_from_affiliation'].isna()
    ]
    
    print(f"Looking up institutions for {len(missing_final)} projects...")
    print("Strategy: ENA center name ? Manual mapping ? ROR API ? Text search")
    institution_count = 0
    ena_success = 0
    manual_mapping_success = 0
    ror_success = 0
    text_success = 0
    
    with tqdm(total=len(missing_final), desc="Institutions") as pbar:
        for idx, row in missing_final.iterrows():
            project_id = row[PROJECT_ID_COL]
            country = None
            source = None
            
            # PRIORITY 1: Fetch center name from ENA
            center_name = fetch_ena_center_name(project_id)
            if center_name:
                df.at[idx, 'ena_center_name'] = center_name  # Store for reference
                
                # Try manual mapping with fuzzy matching (more accurate)
                country = find_best_institution_match(center_name, INSTITUTION_COUNTRY_MAP)
                if country:
                    source = 'ena_manual'
                    manual_mapping_success += 1
                else:
                    # Try ROR lookup
                    country = lookup_institution_country(center_name)
                    if country:
                        source = 'ena_ror'
                        ror_success += 1
                
                if country:
                    df.at[idx, 'country_from_institution'] = f"{country} (ena_center)"
                    institution_count += 1
                    ena_success += 1
                    pbar.set_postfix({
                        'assigned': institution_count, 
                        'source': source,
                        'center': center_name[:20]
                    })
                    pbar.update(1)
                    continue
            
            # PRIORITY 2: Fallback to text description search
            text = row['text_for_search']
            match = inst_pattern.search(text)
            
            if match:
                inst = match.group(1).strip()
                
                # Try manual mapping with fuzzy matching
                country = find_best_institution_match(inst, INSTITUTION_COUNTRY_MAP)
                if country:
                    manual_mapping_success += 1
                else:
                    # Try ROR lookup
                    country = lookup_institution_country(inst)
                    if country:
                        ror_success += 1
                
                if country:
                    df.at[idx, 'country_from_institution'] = f"{country} (text_institution)"
                    institution_count += 1
                    text_success += 1
                    pbar.set_postfix({
                        'assigned': institution_count, 
                        'source': 'text',
                        'inst': inst[:20]
                    })
            
            pbar.update(1)
    
    print(f"[OK] Assigned {institution_count} projects from institutions")
    print(f"  - ENA center names found: {ena_success}")
    print(f"    * Via manual mapping: {manual_mapping_success}")
    print(f"    * Via ROR lookup: {ror_success}")
    print(f"  - Text extraction: {text_success}")
    print_progress_summary(df, "Institution lookup")
    
    # Combine all sources
    print("\n" + "="*70)
    print("COMBINING ALL SOURCES")
    print("="*70)
    df['assigned_country'] = (
        df['country_from_population']
        .fillna(df['country_from_text'])
        .fillna(df['country_from_abstract'])
        .fillna(df['country_from_fulltext'])
        .fillna(df['country_from_affiliation'])
        .fillna(df['country_from_institution'])
        .fillna('Unknown')
    )
    
    # Parse and normalize
    print("\nParsing and normalizing all country assignments...")
    tqdm.pandas(desc="Normalizing")
    df['parsed_countries'] = df['assigned_country'].progress_apply(parse_and_normalize_countries)
    
    # Explode for summary
    exploded_df = df.explode('parsed_countries').copy()
    exploded_df = exploded_df.rename(columns={'parsed_countries': 'clean_country'})
    
    # CRITICAL: Remove any non-ASCII country names (encoding errors)
    print("\nCleaning non-ASCII country names...")
    before_count = len(exploded_df)
    exploded_df = exploded_df[exploded_df['clean_country'].apply(
        lambda x: str(x).isascii() if pd.notna(x) else True
    )]
    removed_count = before_count - len(exploded_df)
    if removed_count > 0:
        print(f"  Removed {removed_count} entries with non-ASCII country names (encoding errors)")
    
    # Aggregated summary
    country_summary = exploded_df.groupby('clean_country').agg(
        total_count=('clean_country', 'size'),
        early_life_count=('is_early_life', 'sum')
    ).reset_index()
    
    country_summary = country_summary.sort_values('total_count', ascending=False)
    
    # Save outputs
    print("\n" + "="*70)
    print("SAVING RESULTS")
    print("="*70)
    
    breakdown_file = 'country_assignment_breakdown.csv'
    country_summary.to_csv(breakdown_file, index=False)
    print(f"[OK] Country breakdown saved: {breakdown_file}")
    
    # Detailed output
    detailed_df = df[[PROJECT_ID_COL, 'assigned_country', 'ena_center_name', 
                      PUBLICATIONS_COL, 'is_early_life']].copy()
    detailed_df['clean_countries_list'] = df['parsed_countries'].apply(
        lambda x: '; '.join(sorted(set(x))) if isinstance(x, list) else str(x)
    )
    
    detailed_df['Related_PMIDs'] = detailed_df[PUBLICATIONS_COL].apply(
        lambda x: ', '.join([p.strip() for p in str(x).split(',') if p.strip().replace('.', '').isdigit()]) if pd.notna(x) else ''
    )
    
    detailed_df = detailed_df.rename(columns={
        PROJECT_ID_COL: 'Project_ID',
        'assigned_country': 'Original_Assigned_Country',
        'is_early_life': 'Is_Early_Life',
        'ena_center_name': 'ENA_Center_Name'
    }).drop(columns=[PUBLICATIONS_COL])
    
    detailed_file = 'detailed_country_assignments.csv'
    detailed_df.to_csv(detailed_file, sep='\t', index=False)
    print(f"[OK] Detailed assignments saved: {detailed_file}")
    
    # Final statistics
    print("\n" + "="*70)
    print("FINAL STATISTICS")
    print("="*70)
    
    total_projects = len(df)
    multi_projects = df['assigned_country'].str.startswith('Multi-country', na=False).sum()
    unknown_projects = (df['assigned_country'] == 'Unknown').sum()
    assigned_projects = total_projects - unknown_projects
    
    print(f"\nTotal projects: {total_projects}")
    print(f"Projects assigned: {assigned_projects} ({assigned_projects/total_projects*100:.1f}%)")
    print(f"Multi-country: {multi_projects} ({multi_projects/total_projects*100:.1f}%)")
    print(f"Unknown: {unknown_projects} ({unknown_projects/total_projects*100:.1f}%)")
    print(f"Early-life: {df['is_early_life'].sum()} ({df['is_early_life'].sum()/total_projects*100:.1f}%)")
    
    print("\nAssignment sources:")
    print(f"  Population column: {pop_assigned}")
    print(f"  Descriptions: {text_assigned}")
    print(f"  Abstracts: {abstract_count}")
    print(f"  Full-text: {fulltext_count}")
    print(f"  Affiliations: {affiliation_count}")
    print(f"  Institutions: {institution_count}")
    if institution_count > 0:
        print(f"    - ENA center names: {ena_success}")
        print(f"    - Text extraction: {text_success}")
    
    # ENA center name statistics
    ena_centers_found = df['ena_center_name'].notna().sum()
    print(f"\nENA center names retrieved: {ena_centers_found}")
    if ena_centers_found > 0:
        print(f"  Successfully mapped to countries: {ena_success} ({ena_success/ena_centers_found*100:.1f}%)")
    
    print("\nTop 15 countries:")
    print(country_summary.head(15).to_string(index=False))
    
    print("\n" + "="*70)
    print("COMPLETE!")
    print("="*70)
    print(f"\nCache saved to: {CACHE_FILE}")
    print("Rerun the script on more data to benefit from cached lookups!")


if __name__ == '__main__':
    main()