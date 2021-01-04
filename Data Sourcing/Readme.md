## Data Scrapping from TMDB: 

We first used the [TMDB API](https://developers.themoviedb.org/3/getting-started/introduction) and the python code in Jupyter Notebook [TMDB_API](https://github.com/ajithgh/Predicting-Movie-success/blob/main/Data%20Sourcing/TMDB_API.ipynb). This code help us Scrape data with 30 features between 2017-2021. 

We then wrote another code in same format to extract the Online content providers for each of our 6.4k records from [here](https://developers.themoviedb.org/3/movies/get-movie-watch-providers)

--------------

## Concatinating Unique Entitites in Excel seperated by Commas.
The following function helps us to concatenate only the unique values into a single cell from a list
  1. Hold down the `ALT + F11` keys to open the Microsoft Visual Basic for Applications window.
  2. Click `Insert > Module`, and paste the [VBA code]() in the Module Window.
  3. Then go back to your worksheet, and enter this formula: `=ConcatUniq (A1:A17,",")` into a blank cell where you want to output the concatenated result, and press Enter key to get the unique concatenated result
  
**Note:**
+ This code will need modifiaction if you have entities seperated by something other than ",". In such cases, modify code accordingly.
+ This code will fail to gove the right answers if the starting cell (say A1) is blank. If you anticiapate to have more starting cells as blank, create a duplicate column/row with entitity "a," and then use `CONCATUNIQ`. Once you finish Concatinating, you can then select the column/row with the results and use `Replace` function to replace "a,," with "" (blank). 
