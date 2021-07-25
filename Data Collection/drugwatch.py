from bs4 import BeautifulSoup
import json
import requests
import pandas as pd
import re

def get_search_info(search_url, pagecount=int(1), postcount=int(1), df=pd.DataFrame()):
    search_page = requests.get(search_url)
    search_soup = BeautifulSoup(search_page.content, 'html.parser')
    urls = [search.get_text() for search in search_soup.find_all('span', class_="search-page__result-url")]

    for post_url in urls:
        post_dict = {}
        
        ## get ID
        post_dict['id'] = postcount

        ## get url
        post_dict['url'] = post_url
        ## make page bs4 inquiry
        post_page = requests.get(post_url)
        post_soup = BeautifulSoup(post_page.content, 'html.parser')
        ## get title
        try:
            if post_soup.find('h1', class_="hero__title") is not None:
                title = post_soup.find('h1', class_="hero__title").get_text()
            else:
                title = post_soup.find(attrs={"data-location" : "page-header"}).find('h1').get_text()
            post_dict['title'] = title
        except:
            post_dict['title'] = ''
        ## get publish time
        try:
            pubtime = post_soup.find('div', class_='post-meta__publish').find('span').find('time').get_text()
            post_dict['publish_time'] = pubtime
        except:
            post_dict['publish_time'] = ''
        ## get abstract
        try:
            abstract = post_soup.find('div', class_="hero__intro").get_text()
            post_dict['abstract'] = abstract
        except:
            post_dict['abstract'] = ''        
        ## get text
        regex = re.compile('.*wysiwyg*')
        all_div = post_soup.find_all("div", {"class" : regex})
        text_list = [div.get_text() for div in all_div]
        text = ''.join(text_list)
        if text == '':
            continue
        post_dict['text'] = text
        
        ## Write to df
        row = pd.Series()
        for k,v in post_dict.items():
            row[k] = v
        df = df.append(row, ignore_index=True)
        
        ## write to json file
        with open('json/drugwatch/drugwatch_{id}.json'.format(id=postcount), 'w') as outfile:
            json.dump(post_dict, outfile)
            
        postcount += 1

    ## if next page exists, repeat the process on the nex† page    
    if search_soup.find('a', class_="pagination__el pagination__el--link pagination__el--next") is not None:
        pagecount += 1
        search_url = 'https://www.drugwatch.com/search/?query=medical%20malpractice&page_number={pg}'.format(pg=pagecount)
        df = get_search_info(search_url, pagecount = pagecount, postcount=postcount, df = df)
    
    return df