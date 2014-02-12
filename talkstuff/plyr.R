ultraURL <- 'http://ultrasignup.com/entrants_event.aspx?did=26838'
x <- htmlParse(getURL(ultraURL,.opts=ultraOpts()),asText=TRUE)
getNodeSet(x,'//a[@class="event_selected_link"]')
