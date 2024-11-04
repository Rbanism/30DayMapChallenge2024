# 30DayMapChallenge2024 by Rbanism

The [#30DayMapChallenge](https://30daymapchallenge.com/) is an event which gathers mappers on social media each November since 2019. The challenge is to produce a map for each of the 30 days of November, following the set of themes / topics / constraints set by the organisers. The list of themes for November 2024 has just been published!


This year, we would like to participate in the **#30DayMapChallenge** as a group: Rbanism. Given our community focus on the programming language R, we add a constraint to the challenge: **each map has to be made using R**. To make it more manageable, we spread the task of producing 30 maps among ourselves, and we provide inspiration and data by including BK bachelor, MSc Urbanism and Landscape Architecture students' GIS production. That way, if you lack inspiration, your task is *only* to reproduce the map using with R.

If you would like to join the challenge, you can:
1. You pick a topic that interests you and put your name next to it [here](https://docs.google.com/spreadsheets/d/1YEGjTCaki1-eSwPiO6DEi5-kWVbGZF_MznGGCCHTlis/edit?usp=sharing) (if you haven’t already). We welcome multiple maps or map-makers per topic, so feel free to work on any topic that you wish (…but if you like a topic with no name on it, that’s even better and maybe we can complete the challenge with a map for each day)
2. You make your map (or try to make the make. You can find resources on our GitHub page or reach out if you’re stuck) then, *at least one day before the date of your topic,* A) you send it to [rbanism@tudelft.nl](mailto:rbanism@tudelft.nl) or B) you send a Pull Request to [GitHub](https://github.com/Rbanism/30DayMapChallenge2024). In any case, please make sure your map contains the Rbanism logo, the #30DayMayChallenge, **appropriate data sources, licenses, and author credits (This is crucial for sharing our work!).** See [here](https://github.com/Rbanism/30DayMapChallenge2024?tab=readme-ov-file#how-to-overlay-our-logo-onto-a-ggplot-map) how to add these to a ggplot map.
3. We’ll post the maps weekly on [our instagram](https://www.instagram.com/rbanism_?utm_source=ig_web_button_share_sheet&igsh=ZDNlZDc0MzIxNw==) account and on working days on our BlueSky account [@rbanism.bsky.social](https://bsky.app/profile/rbanism.bsky.social) with the hashtags #Rbanism #30DayMapChallenge2024 #30DayMapChallenge2024_Rbanism. We will also keep an archive of the challenge for our website [rbanism.org](http://rbanism.org), and try to organise a mini-exhibition at the Faculty sometime in December.

This can be a **great opportunity to work on your mapping skills with R, to experiment with new packages and new functions, and to link with fellow mappers at TU Delft**. We're looking forward to see what Rbanism comes up with as a community, and will host the result on our [website](rbanism.org).

## Resources
- [Geospatial Data Carpentry for Urbanism](https://carpentries-incubator.github.io/r-geospatial-urban/) provides a comprehensive guide on how you can work with raster and vector data in R.

### Our logo
![](https://rbanism.org/assets/imgs/about/vi_l.jpg)

### Our colour Hex codes
Purple: `#93278F`
Teal: `#00A99D`
Orange: `#F7931E`

### How to overlay our logo onto a ggplot map?

```{r}
#install.packages(c("magick","grid"))     # install if needed
library(magick)
library(grid)

rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg') # Download our logo

ggplot()                                   # make your map

grid.raster(rbanism_logo, x = 0.9, y=0.9,  # x and y determine the position of the logo (top right)
            width = unit(100, "points"))   # width determines the size of the logo
```

## Guidelines for production

1. Check the [presentation](https://rbanism.github.io/30DayMapChallenge2024/), [template](https://rbanism.github.io/30DayMapChallenge2024/template/30daysmapchallenge_template_example.html) and map [examples](https://rbanism.github.io/30DayMapChallenge2024/8Nov_HDX/8Nov_HDX.html) on GitHub.
2. You can use Rbanism color [hexcodes](https://github.com/Rbanism/30DayMapChallenge2024?tab=readme-ov-file#our-colour-hex-codes) for visual references,  but feel free to use other colors of your preference
3. Make sure your map includes the challenge date and topic (ideally as part of the title), author credits, data sources, licenses, **the Rbanism logo and #30DayMayChallenge hashtag**. See our examples [here](https://rbanism.github.io/30DayMapChallenge2024/8Nov_HDX/8Nov_HDX.html) and [here](https://rbanism.github.io/30DayMapChallenge2024/15Nov_MyData/15Nov_MyData.html).
4. Export you map into a `.png` image. You can use the `ggsave` function if using the `ggplot2` package for mapping. See our examples [here](https://rbanism.github.io/30DayMapChallenge2024/8Nov_HDX/8Nov_HDX.html) and [here](https://rbanism.github.io/30DayMapChallenge2024/15Nov_MyData/15Nov_MyData.html).

## Guidelines for submission

1. Create a folder whose title is “*challengedate_challengetopic”,* containing: 1) the data  2) R code 3) PNG of the map. See our examples [here](https://github.com/Rbanism/30DayMapChallenge2024/tree/main/8Nov_HDX) and [here](https://github.com/Rbanism/30DayMapChallenge2024/tree/main/15Nov_MyData).
2. Send the zipped folder to [rbanism@tudelft.nl](mailto:rbanism@tudelft.nl) or 2) send a Pull Request on [GitHub](https://github.com/Rbanism/30DayMapChallenge2024) *at least one day before the date of your topic.*
3. Collect your chocolate in the advent calendar taped to the #30DayMapChallenge poster in the urbanism coffee area (close to the room we had our kick-off). **You deserve it!**

**If you want your author credits to be shared on our social media, please share your instagram handle with the submission! If via email, simply add it to the mail, if via GitHub, please add a .txt file within the folder.*

## #30DayMapChallenge on social media 

On BlueSky
- The Challenge [feed](https://bsky.app/profile/did:plc:bjm7fq6jgotowpim5ggfbzw6/feed/aaaiqkbjq3yhy)
- [@Rbanism](https://bsky.app/profile/rbanism.bsky.social)

On Instagram
- [@Rbanism_](https://www.instagram.com/rbanism_?utm_source=ig_web_button_share_sheet&igsh=ZDNlZDc0MzIxNw==)


# Any questions or ideas?

If there any ideas, questions, or uncertainties regarding the challenge, please don’t hesitate to reach out to us via [rbanism@tudelft.nl](mailto:rbanism@tudelft.nl) !

=======

![30DayMapChallenge2024](https://30daymapchallenge.com/imgs/30dmc_2024.png)

```
