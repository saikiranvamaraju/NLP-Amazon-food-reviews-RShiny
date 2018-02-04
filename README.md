# NLP-Amazon-food-reviews-RShiny
NLP on Amazon food reviews using R Shiny. 

Dataset-  https://www.kaggle.com/snap/amazon-fine-food-reviews -
Amazon food reviews on kaggle was the dataset used for this project. The data had 500K reviews from various customers.

Background-
The reviews for normalized by converting uppercase to lowercase, and removing stop words and punctuations. Word clouds with 200 most frequently occuring words were built before and after normalizing the reviews for comparison. Using AFINN-111, sentiment score for each review was calculated by summing up score of each word. A positive score indicates a positive sentiment, a negative score indicates a
negative sentiment. The magnitude of the score indicates the degree of sentiment. For each of the top 6 most reviewed products, scatter plots with sentiment score per review on the x-axis and the Rating as provided per reviewer on the y-axis were visualized. Also, correlation between the sentiment score and the product rating was obtained.

All the tables,results and plots were finally visualized in Shiny.

