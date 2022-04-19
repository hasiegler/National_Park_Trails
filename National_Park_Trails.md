Analysis of National Park Trails
================
Henry Siegler

``` r
library(tidyverse)
library(formattable)
```

### The Data

The data below contains data on every trail in the National Parks
Service, with information gathered from alltrails.com. [Link to
data](https://www.kaggle.com/datasets/planejane/national-park-trails).

``` r
np_trails <- read_csv(here::here("national_park_trails.csv"))
```

``` r
#This is how the data is formatted
np_trails %>% 
  head(n = 2) %>% 
  formattable()
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:right;">
trail_id
</th>
<th style="text-align:right;">
name
</th>
<th style="text-align:right;">
area_name
</th>
<th style="text-align:right;">
city_name
</th>
<th style="text-align:right;">
state_name
</th>
<th style="text-align:right;">
country_name
</th>
<th style="text-align:right;">
\_geoloc
</th>
<th style="text-align:right;">
popularity
</th>
<th style="text-align:right;">
length
</th>
<th style="text-align:right;">
elevation_gain
</th>
<th style="text-align:right;">
difficulty_rating
</th>
<th style="text-align:right;">
route_type
</th>
<th style="text-align:right;">
visitor_usage
</th>
<th style="text-align:right;">
avg_rating
</th>
<th style="text-align:right;">
num_reviews
</th>
<th style="text-align:right;">
features
</th>
<th style="text-align:right;">
activities
</th>
<th style="text-align:right;">
units
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
10020048
</td>
<td style="text-align:right;">
Harding Ice Field Trail
</td>
<td style="text-align:right;">
Kenai Fjords National Park
</td>
<td style="text-align:right;">
Seward
</td>
<td style="text-align:right;">
Alaska
</td>
<td style="text-align:right;">
United States
</td>
<td style="text-align:right;">
{‘lat’: 60.18852, ‘lng’: -149.63156}
</td>
<td style="text-align:right;">
24.8931
</td>
<td style="text-align:right;">
15610.598
</td>
<td style="text-align:right;">
1161.8976
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
out and back
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
423
</td>
<td style="text-align:right;">
\[‘dogs-no’, ‘forest’, ‘river’, ‘views’, ‘waterfall’, ‘wild-flowers’,
‘wildlife’\]
</td>
<td style="text-align:right;">
\[‘birding’, ‘camping’, ‘hiking’, ‘nature-trips’, ‘trail-running’\]
</td>
<td style="text-align:right;">
i
</td>
</tr>
<tr>
<td style="text-align:right;">
10236086
</td>
<td style="text-align:right;">
Mount Healy Overlook Trail
</td>
<td style="text-align:right;">
Denali National Park
</td>
<td style="text-align:right;">
Denali National Park
</td>
<td style="text-align:right;">
Alaska
</td>
<td style="text-align:right;">
United States
</td>
<td style="text-align:right;">
{‘lat’: 63.73049, ‘lng’: -148.91968}
</td>
<td style="text-align:right;">
18.0311
</td>
<td style="text-align:right;">
6920.162
</td>
<td style="text-align:right;">
507.7968
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
out and back
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4.5
</td>
<td style="text-align:right;">
260
</td>
<td style="text-align:right;">
\[‘dogs-no’, ‘forest’, ‘views’, ‘wild-flowers’, ‘wildlife’\]
</td>
<td style="text-align:right;">
\[‘birding’, ‘camping’, ‘hiking’, ‘nature-trips’, ‘walking’\]
</td>
<td style="text-align:right;">
i
</td>
</tr>
</tbody>
</table>

**National Parks with Most 5 Star Rated Trails **

``` r
np_trails %>% 
  filter(avg_rating == 5) %>% 
  group_by(area_name) %>%
  count() %>% 
  as.data.frame() %>% 
  slice_max(n, n = 5)
```

    ##                      area_name  n
    ## 1       Yosemite National Park 71
    ## 2 Rocky Mountain National Park 41
    ## 3   Grand Canyon National Park 40
    ## 4        Glacier National Park 37
    ## 5        Olympic National Park 33

**Question 1.2**

``` r
np_trails %>% 
  mutate(length_miles = length / 1609.34) %>% 
  slice_max(length_miles, n = 3) %>%
  pull(length_miles, name)
```

    ##        Olympic Peninsula Loop Drive                     John Muir Trail 
    ##                               329.2                               211.0 
    ## John Muir Trail via Yosemite Valley 
    ##                               203.1

**Question 1.3**

``` r
np_trails %>% 
  group_by(area_name) %>% 
  slice_max(popularity) %>% 
  pull(name, area_name)
```

    ##                                            Acadia National Park 
    ##                                        "The Beehive Loop Trail" 
    ##                                            Arches National Park 
    ##                                           "Delicate Arch Trail" 
    ##                                          Badlands National Park 
    ##                                                   "Notch Trail" 
    ##                                          Big Bend National Park 
    ##                                               "Lost Mine Trail" 
    ##                                          Biscayne National Park 
    ##                                   "Biscayne National Park Walk" 
    ##                      Black Canyon of the Gunnison National Park 
    ##                                     "Warner Point Nature Trail" 
    ##                                      Bryce Canyon National Park 
    ##                          "Navajo Loop and Queen's Garden Trail" 
    ##                                       Canyonlands National Park 
    ##                                               "Mesa Arch Trail" 
    ##                                      Capitol Reef National Park 
    ##                                          "Hickman Bridge Trail" 
    ##                                  Carlsbad Caverns National Park 
    ##                          "Carlsbad Caverns National Park Trail" 
    ##                                   Channel Islands National Park 
    ##                                           "Potato Harbor Trail" 
    ##                     Clayton Co International Park, Jonesboro GA 
    ##                       "Clayton County International Park Trail" 
    ##                                          Congaree National Park 
    ##                                          "Boardwalk Loop Trail" 
    ##                               Congaree National Park Wilderness 
    ##                                                   "Cedar Creek" 
    ##                                       Crater Lake National Park 
    ##                                           "Garfield Peak Trail" 
    ##                                   Cuyahoga Valley National Park 
    ##                                                  "Ledges Trail" 
    ##                                      Death Valley National Park 
    ##                               "Badwater Basin Salt Flats Trail" 
    ##                                            Denali National Park 
    ##                                    "Mount Healy Overlook Trail" 
    ##                                      Dry Tortugas National Park 
    ##                                           "Fort Jefferson Loop" 
    ##                                        Everglades National Park 
    ##                                            "Shark Valley Trail" 
    ##                                         Fort Hunt National Park 
    ##                                            "Mount Vernon Trail" 
    ##                                      Fort Pickens National Park 
    ##                                            "Fort Pickens Trail" 
    ##                                      Gateway Arch National Park 
    ##                                    "St. Louis Riverfront Trail" 
    ##                                       Glacier Bay National Park 
    ##                               "Bartlett Cove Forest Loop Trail" 
    ##                                           Glacier National Park 
    ##                                        "Grinnell Glacier Trail" 
    ##                                      Grand Canyon National Park 
    ## "Bright Angel Trail to Bright Angel Campground and River Trail" 
    ##                                       Grand Teton National Park 
    ##                                          "Cascade Canyon Trail" 
    ##                                       Great Basin National Park 
    ##                      "Wheeler Peak Trail via Stella Lake Trail" 
    ##                     Great Sand Dunes National Park and Preserve 
    ##                                               "High Dune Trail" 
    ##                             Great Smoky Mountains National Park 
    ##                              "Alum Cave Trail to Mount LeConte" 
    ##                               Guadalupe Mountains National Park 
    ##                          "Guadalupe Peak Texas Highpoint Trail" 
    ##                                         Haleakala National Park 
    ##                               "Pipiwai Trail and Waimoku Falls" 
    ##                                  Hawaii Volcanoes National Park 
    ##                        "Kilauea Iki Trail and Crater Rim Trail" 
    ##                                       Hot Springs National Park 
    ##                                                  "Sunset Trail" 
    ##                                     Indiana Dunes National Park 
    ##                                              "Cowles Bog Trail" 
    ##                                       Isle Royale National Park 
    ##               "Greenstone Ridge Trail: Windigo to Tobin Harbor" 
    ##                                       Joshua Tree National Park 
    ##                                           "Ryan Mountain Trail" 
    ##                                            Katmai National Park 
    ##                                                  "Brooks Falls" 
    ##                                      Kenai Fjords National Park 
    ##                                       "Harding Ice Field Trail" 
    ##                                      Kings Canyon National Park 
    ##                                              "Mist Falls Trail" 
    ##                                   Lassen Volcanic National Park 
    ##                                                   "Lassen Peak" 
    ##                                      Mammoth Cave National Park 
    ##                                      "Green River Bluffs Trail" 
    ##                                        Mesa Verde National Park 
    ##                                        "Petroglyph Point Trail" 
    ##                                     Mount Rainier National Park 
    ##                                             "Tolmie Peak Trail" 
    ##                                    North Cascades National Park 
    ##                                               "Blue Lake Trail" 
    ##                                           Olympic National Park 
    ##                            "Hurricane Hill via Hurricane Ridge" 
    ##                                  Petrified Forest National Park 
    ##                                               "Blue Mesa Trail" 
    ##                                         Pinnacles National Park 
    ##                   "Condor Gulch Trail to High Peaks Trail Loop" 
    ##                                           Redwood National Park 
    ##                                   "Tall Trees Grove Loop Trail" 
    ##                                    Rocky Mountain National Park 
    ##                                            "Emerald Lake Trail" 
    ##                                           Saguaro National Park 
    ##                                     "Bridal Wreath Falls Trail" 
    ##                                           Sequoia National Park 
    ##                                               "Moro Rock Trail" 
    ##                                        Shenandoah National Park 
    ##                                         "Old Rag Mountain Loop" 
    ##                                Theodore Roosevelt National Park 
    ##                                             "Wind Canyon Trail" 
    ##                                         Voyageurs National Park 
    ##                                           "Blind Ash Bay Trail" 
    ##                                         Wind Cave National Park 
    ##                               "Rankin Ridge Interpretive Trail" 
    ##                 Wolf Trap National Park for the Performing Arts 
    ##                                          "Wolf Trap Loop Trail" 
    ##                                       Yellowstone National Park 
    ##            "Mystic Falls, Fairy Creek and Little Firehole Loop" 
    ##                                          Yosemite National Park 
    ##                    "Vernal and Nevada Falls via the Mist Trail" 
    ##                                              Zion National Park 
    ##                                          "Angels Landing Trail"

**Question 1.4**

``` r
np_trails %>% 
  distinct(area_name) %>% 
  pull(area_name) %>% 
  str_detect("Canyon") %>% 
  sum()
```

    ## [1] 5

**Question 1.5**

``` r
all_words <- np_trails %>% 
  pull(name) %>% 
  str_c(collapse = " ") %>% 
  str_to_lower() %>% 
  str_split(" ") %>% 
  as.data.frame(col.names = "single_word")

all_words <- all_words %>% 
  filter(single_word != "trail",
         single_word != "to",
         single_word != "loop",
         single_word != "and",
         single_word != "via",
         )

all_words %>% 
  group_by(single_word) %>% 
  count(sort = TRUE) %>% 
  as.data.frame() %>% 
  slice_max(n, n = 5) %>% 
  pull(single_word)
```

    ## [1] "lake"     "creek"    "mountain" "falls"    "canyon"

**Question 1.6**

``` r
recommend_trails <- function(dataset, park, min_len_mi = 0, 
                             max_len_mi = NULL, min_rating = 0, type = NULL){
  
  stopifnot(is.data.frame(dataset),
            is.character(park),
            is.numeric(min_len_mi),
            is.numeric(max_len_mi) | is.null(max_len_mi),
            is.numeric(min_rating),
            is.character(type) | is.null(type)
            )
  
  dataset <- dataset %>% 
    mutate(length_miles = length / 1609.34)
  
  dataset <- dataset %>% 
    filter(area_name == park,
           length_miles >= min_len_mi,
           avg_rating >= min_rating)
  
  if (is.null(max_len_mi)) {
    dataset <- dataset
  } else if (is.numeric(max_len_mi)) {
    dataset <- dataset %>% 
      filter(length_miles <= max_len_mi)
  }
  
  if (is.null(type)) {
    dataset <- dataset
  } else if (is.character(type)) {
    dataset <- dataset %>% 
      filter(route_type == type)
  }

  dataset %>% 
    pull(name)
}
```

``` r
np_trails %>%
  recommend_trails("Yellowstone National Park", 
                   min_len_mi = 10, 
                   min_rating = 4, 
                   type = "loop")
```

    ##  [1] "Sky Rim Loop Trail"                             
    ##  [2] "Hellroaring Creek Loop Trail"                   
    ##  [3] "Sportsman Lake Crescent High Lake Trail Loop"   
    ##  [4] "Lewis River Channel Loop"                       
    ##  [5] "Delacy Creek Trail to Shoshone Lake"            
    ##  [6] "Sepulcher Mountain Trail"                       
    ##  [7] "Bunsen Peak Loop"                               
    ##  [8] "Mystic Falls to Ferry Creek Loop"               
    ##  [9] "Heart Lake Loop Trail"                          
    ## [10] "Pelican Creek Loop Trail"                       
    ## [11] "Hayden’s Valley Loop"                           
    ## [12] "Falls River Basin Trail"                        
    ## [13] "Heart Lake and the Two Ocean Plateau Loop Trail"
    ## [14] "Sentinel Meadows and Fairy Falls Loop"

**Question 1.7**

``` r
parks_near_SLO <- c("Sequoia National Park",
                    "Joshua Tree National Park",
                    "Lassen Volcanic National Park",
                    "Kings Canyon National Park",
                    "Pinnacles National Park", 
                    "Death Valley National Park")

np_trails %>% 
  map(parks_near_SLO, 
      recommend_trails, 
      dataset = .,
      min_len_mi = 20, 
      max_len_mi = 30, 
      min_rating = 4.5, 
      type = "loop")
```

    ## [[1]]
    ## [1] "Mineral King to Little Five Lakes Trail"
    ## [2] "Mineral King: Black Rock Pass Loop"     
    ## 
    ## [[2]]
    ## character(0)
    ## 
    ## [[3]]
    ## [1] "Butte Lake, Snag Lake, Twin Lake, Silver Lake Loop"                               
    ## [2] "Summit Lake, Horshoe Lake, Snag Lake, Cinder Cone, Twin Lakes, and Echo Lake loop"
    ## 
    ## [[4]]
    ## character(0)
    ## 
    ## [[5]]
    ## character(0)
    ## 
    ## [[6]]
    ## character(0)
