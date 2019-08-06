library("RedditExtractoR")
library("quanteda")
set.seed(221186)

brexit_reddit_urls <- reddit_urls(subreddit = "brexit", page_threshold = 4)
brexit_reddit_urls <- brexit_reddit_urls[brexit_reddit_urls$num_comments>10,]
brexit_reddit_content <- reddit_content(brexit_reddit_urls$URL, wait_time = 2)

kwic(brexit_reddit_content$comment, phrase("no deal"))
kwic(brexit_reddit_content$comment, phrase("ireland"))

sample(brexit_reddit_content$comment,3)
brexit_reddit_content$outcome <- brexit_reddit_content$comment_score<0
brexit_corpus <- corpus(brexit_reddit_content, text_field = "comment")

brexit_dfm <- dfm(brexit_corpus, remove = stopwords("en"), remove_punct = T, remove_numbers = T)
brexit_dfm <- dfm_trim(brexit_dfm, min_termfreq = 3)

textplot_wordcloud(brexit_dfm_con)
tmp_mat <- as.matrix(brexit_dfm)

library(glmnet)

lasso_out <- cv.glmnet(x= tmp_mat, y = docvars(brexit_dfm)$outcome, nfolds = 5)
lasso_coefs <- as.matrix(coef(lasso_out,lasso_out$lambda.1se))[,1]
lasso_coefs[lasso_coefs!=0]
table(lasso_coefs==0)

graph_object <- construct_graph(brexit_reddit_content)

str(another)
test$num_comments
str(test)