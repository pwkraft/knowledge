### old sophistication measures etc.


## diversity in item response
anes2012$ditem <- apply(anes2012spell[,-1], 1, function(x){
  iwc <- unlist(lapply(strsplit(x,"\\s+"), length))
  p <- iwc/sum(iwc)
  plp <- p * log2(p)
  plp[is.na(plp)] <- 0
  entrop <- - sum(plp)
  return(entrop)
})

## topic diversity score
data$topic_diversity <- -1 * rowSums(doc_topic_prob * log2(doc_topic_prob))

## weighted topic diversity score, by length
data$topic_diversity_length <- with(data, (topic_diversity + 1) * (lwc + 1))

## threshold topic probability scores
doc_topic_prob_sparse <- doc_topic_prob
doc_topic_prob_sparse[doc_topic_prob_sparse < 0.1] <- 0
doc_topic_prob_binary <- doc_topic_prob
doc_topic_prob_binary[doc_topic_prob_binary < 0.1] <- 0
doc_topic_prob_binary[doc_topic_prob_binary > 0.1] <- 1

## count over thresholded prob scores
data$topic_thresh_sum <- rowSums(doc_topic_prob_sparse)
data$topic_thresh_count <- rowSums(doc_topic_prob_binary)

## weighted by item count + diversity
data$topic_diversity_length_litem <- with(data, (topic_diversity + 1) * (lwc + 1) * (litem + 1))
data$topic_diversity_length_ditem <- with(data, (topic_diversity + 1) * (lwc + 1) * (ditem + 1))
