#' Get out the vote experiment
#'
#' An experiment about ways to encourage voting in primary elections. During the 2006
#' primary election in Michigan, registered voters were randomly assigned to different
#' treatments, each in the form of a postcard mailed to them before the primary. The most high-pressure
#' message("Neighbors")
#' listed the voters neighbors and whether they voted in the previous primary elections. The card promised to
#' send out the same information after the 2006 primary, so that "you and your neighbors will all know who voted
#' and who did not." (From the Gerber et al. reference, below.) In another treatment, "Civic Duty," the postcard said, "On August 8,
#' remember your rights and responsibilities as a citizen. Remember to vote. DO YOUR CIVIC DUTY---VOTE!" Yet another
#' treatment, "Hawthorne" simply told the voter that "YOU ARE BEING STUDIED!" as part of research on why people do or do not vote. There
#' was also a control group that did not receive a postcard.
#'
#' @usage data(Go_vote)
#'
#' @format A data frame with 305866 rows and 6 variables:
#'
#' - sex of the voter (female or male)
#' - yearofbirth: year of birth of the voter
#' - primary2004: whether the voter voted in the 2004 primary election (voted, abstained)
#' - messages: Get-out-the-vote message the voter received (Civic Duty, Control, Neighbors, Hawthorne)
#' - primary2006: whether the voter turned out in the 2006 primary election (voted, abstained)
#' - hhsize: household size of the voter
#'
#' @references
#' - Imai, Kosuke. 2017. Quantitative Social Science: An Introduction. Princeton University Press. URL.
#'
#' - Alan S. Gerber, Donald P. Green, and Christopher W. Larimer (2008) “Social pressure and voter turnout: Evidence from a large-scale field experiment.” American Political Science Review, vol. 102, no. 1, pp. 33–48. doi: 10.1017/S000305540808009X
"Go_vote"
