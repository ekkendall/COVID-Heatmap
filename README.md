# Covid Health Hotline Finder

## **Description**
For a (COVID-19) Pandemic Response Hackathon, we took local health county cases/deaths data on COVID-19 (using [The New York Times data](https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html)) and on local health departments (using [NACCHO](https://www.naccho.org/membership/lhd-directory?searchType=standard&lhd-state=ID#card-filter)) and created a Twilio bot, exposed it as an API, and built a dynamic visualization in R Studio with Leaflet. 

## **Landing Page**
The landing page for all three technologies can be accessed [here](http://covid19.georgesaieed.com). The repository for it can be found [here](http://github.com/saieedgeorge0/covid-hotline-bling-landing).

## **Visualization**
This is the repository for the visualization. [Click here](http://ekkendall.github.io/COVID-Heatmap) to view the visualization. Created with Leaflet in R Studio, this allows you to view the cases/deaths by county due to COVID-19. Data comes from The New York Times, and it is automatically updated everytime they add new data. Heatmap was generated using logarathmic transformation of the data, in order to generate a better visual despite the presence of any outliers. Cases matched to unknown counties were removed. The New York Times data contains data for all boroughs associated with New York City as one data point - thus, each of these boroughs shows the aggregate data for all of NYC as associated with them. As per the data, cases associated with Kansas City are not shown. All Chicago cases are included with Cooke County. For other limitations, see the [github repository](https://github.com/nytimes/covid-19-data) associated with the data.

## **Twilio Bot**
[Click here](http://github.com/saieedgeorge0/covid-hotline-bling) to view the repository for the twilio bot. It prompts a user for their location (via zipcode), then gives the user information (from NACCHO) about the local health department closest to them. It also provides them with the most up-to-date data (from The New York Times) regarding the number of cases of and deaths due to COVID-19. It was built with Node.JS and Twilio and is deployed via Heroku. Because we're using the free tier of Heroku, sometimes, the bot goes to sleep - if you don't get a reponse the first time, wait a moment and try again.

## **API**
[Click here](http://github.com/saieedgeorge0/covid-hotline-bling) to view the repository for the Node.JS application that serves as an API which allows you to access The New York Times cases/deaths data for COVID-19 by county given a zip code. Using it is easy - make a GET request:

```html
http://covid-hotline-bling.herokuapp.com/zipcode/44106
```

It will return a JSON object with the FIPS code for the county, number of cases, deaths, and the last time the data was updated:

```json
{
    "fips":"39035", 
    "cases":330, 
    "deaths":2, 
    "recentdate":"2020-03-27"
}
```

## **Contributors**
- [George Saieed](http://georgesaieed.com)
- [Ellen Kendall](http://github.com/ekkendall)

Special thanks to Graham Novak and Jack Mousseau for their advice/help with ideation.