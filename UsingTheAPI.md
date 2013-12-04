Using the API
=============


The APIs support REST-formatted requests. Responses are returned in JSON or XML format, depending upon the content type specified in the request header.
 
* JSON Response = Content-Type:application/json
* XML Response = Content-Type:text/json
 
Below are sample snippets of code that access the [ARMS]() API with jQuery and Python.


Sample jQuery Code
------------------

The following example is for a JSON-formatted response written with jQuery v1.9.1:
 
```javascript
  <html>
    <title>USDA ERS API web service call using jSON</title>
        <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" type="text/javascript"></script>
        <script type="text/javascript">
  
          /* CODE SAMPLE: retrieve list of ARMS reports using ERS API */**_
          function GetARMS() {       
            jQuery.support.cors = true;
        
            var jsonp_url = "http://api.data.gov/USDA/ERS/data/Arms/Reports?api_key=API_KEY&survey=Crop&callback=?";
               
            $.ajax({
              url: jsonp_url,                    
              type: 'GET',           
              dataType: 'jsonp', 
              jsonpCallback: 'jsonCallback',                
              crossDomain: true,  
              success: function() { console.log("success"); }, 
              error: function() { console.log("error"); }                  
            });
          }
        
          function jsonCallback(data) {
               //alert('in jsonCallback');
            var reports = data.dataTable; // this array has the data
            alert(reports);      
            var strResult = "<table><th>Report Number</th><th>Report Description</th>";
    
            $.each(reports, function (index, Report) {
              strResult += "<tr><td>" + Report.report_num + "</td><td> " + Report.report_header + "</td></tr>";
            });
            strResult += "</table>";  
            alert(strResult);     
            
            $("#divResultARMS").html(strResult);         
          }
 
        </script>
      
      <body>
        <h3>USDA ERS API demo </h3>
        <p><button onClick="GetARMS();return false;">ARMS Surveys</button></p>
        <div id="divResultARMS"></div>
      </body>
  </html>
```


Sample Python Code
------------------

The following example is for a JSON-formatted response written with Python:

__pyusda.py__
```python
import sys
from collections import namedtuple

import requests


class APIObject(object):
    """
API Objects pulls data and stores the response.
"""

    def __init__(self, api_url, api_key):

        self.url = '%s?api_key=%s' % (api_url, api_key)
        self.response = requests.get(self.url)
        self.data = self.response.json()


if __name__ == "__main__":
    """
Run APIObject from the commandline:
$ python pyusda.py http://api.data.gov/USDA/ERS/data/Arms/Surveys put_your_api_key_here

The first arg is the api_url. The second is your API_KEY.
$ python pyusda.py $API_URL $API_KEY

Where $API_URL is the url that you want to hit and $API_KEY is replaced with your API_KEY.

If you would like to capture the output run the following:
$ python pyusda.py 'data/Arms/Surveys' $API_KEY > output.json

"""
    import pprint
    ap = APIObject(sys.argv[1], sys.argv[2])
    pprint.pprint(ap.data)
```

__Run APIObject from the commandline__
```
$ python pyusda.py http://api.data.gov/USDA/ERS/data/Arms/Surveys put_your_api_key_here
```

The first arg is the api_url. The second is your API_KEY.
```
$ python pyusda.py $API_URL $API_KEY
```
Where $API_URL is the url that you want to hit and $API_KEY is replaced with your API_KEY.

If you would like to capture the output run the following:
```
$ python pyusda.py 'data/Arms/Surveys' $API_KEY > output.json
```
The output.json file in this repo has an example of how the output would be stored.


__Example of usage from ipython, a python interpretor.__
```python
In [1]: api_key = 'put_your_api_key_here'

In [2]: from pyusda import APIObject

In [3]: ap = APIObject('http://api.data.gov/USDA/ERS/data/Arms/Surveys', api_key)

In [4]: ap.url
Out[4]: 'http://api.data.gov/USDA/ERS/data/Arms/Surveys?api_key=wpe3bvzE0cO9VpVMSCfo6ULSq4ecjy2BKVZ6sOvF'

In [5]: ap.response
Out[5]: <Response [200]>

In [6]: ap.data
Out[6]:
{u'dataTable': [{u'surveyDesc': u'Crop production practices',
   u'survey_abb': u'CROP'},
  {u'surveyDesc': u'Farm finances', u'survey_abb': u'FINANCE'}],
 u'infoTable': [{u'message': u'NO ERROR', u'recordCount': 2}]}

In [7]: ap.data['dataTable']
Out[7]:
[{u'surveyDesc': u'Crop production practices', u'survey_abb': u'CROP'},
 {u'surveyDesc': u'Farm finances', u'survey_abb': u'FINANCE'}]

In [8]: for survey in ap.data['dataTable']:
   ...:         print survey
   ...:
{u'survey_abb': u'CROP', u'surveyDesc': u'Crop production practices'}
{u'survey_abb': u'FINANCE', u'surveyDesc': u'Farm finances'}
```
