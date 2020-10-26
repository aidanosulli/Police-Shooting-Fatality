# Police-Shooting-Fatality

## Background

This project began as a group assignment as part of a data science competition in UCLA Statistics 101C, Intro to Statistical Models and Data Mining. Professor Akram Almohalwas assigned three original datasets to use, with about 30% missing data overall, but allowed teams to use outside data as well. The assignment was to predict victim Fatality from any of the 30+ variables, using whatever models we could use to extract information from the categorical and numeric variables. 

However, after a poor competition performance (though I still got an A in the class, woohoo), this project became a personal vendetta for me to settle. I simply couldn't sit still knowing that other teams had achieved a higher classification acurracy than my group, when I knew that my group had busted its butt for the classification score we did get (officially, our score was 84% classification accuracy)

Therefore, a month after the competition ended, I reopened this cold case, and started the project over from scratch. Much of the data cleaning code for significant variables was reusable; still, I wanted to give all variables a fair second chance to make the final cut, so after double checking all variables, good and bad, I performed feature selection on the entire batch. Additionally, I imputed used a different imputation package, then re-ran my models. 


## Goal

Predict victim fatality from 76,000 violent police encounters from 32 cities nationwide. 

## Phase 1  -*Group Project*-

Code available in Group Project Folder 

**Please Note** The group's distribution of was overall very even, and I believe all parties were left happy. I try to stress parts I did indepentelt, but overall, it was a wonderful team effort. I would be happy to speak about my contributions to any of the areas. 

Working with three other Stats majors at UCLA, this first phase of the project was pivotal to the speed of my success in phase two. As the project was both a competition and a course requirement, my group and I completed several tasks, along with the overarching goal. Our step by step process was as follows: 
#### Step 1: Merge all Data Tables 
   First, we merged all given tables on the available foreign keys, in this case, City and State. 
#### Step 2: Clean Data
   Since the data was originally scraped from handwritten PDF's, nearly every column needed tinkering - changing character columns to categorical/numeric,          combining categorical columns with too many factor levels, using Regex to delete unnecessary character values, etc. This was by far the hardest step, but        fortunately, not a single obersvation had to be dropped. 
#### Step 3: Impute Data
   We imputed around 35,000 missing values total (recall there were initially 76,000 observations across 34 variables) using R's Amelia Package, which uses a        neural network to predict both numeric and categorical values that keep as much variance as possible in the data. 
#### Step 4: Add new Data
   In total, we added 5 extra features to our dataset, ranging from city population to prevelance of gun dealers.
#### Step 5: Modelling
   We applied several models. See graphic below: 
#### Step 6: Present Findings/Results of all previous steps in front of class
   Finally, we presented our all the above steps and results to our class, emphasizing the extra data we scraped and the our parameter selections for our XGBoos    and Random Forest Models. 
   
## Phase 2  -*Solo*-

Given our relatively poor competition performance but the excess of our work, I knew that a major breakthrough in the data was near. As it turned out, I had to re-clean all the data, reusing some of the code but 


## Conclusion
