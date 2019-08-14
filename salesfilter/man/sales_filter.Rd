\name{sales_filter}
\alias{sales_filter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Regular sales filter function
}
\description{
Function which transform sales
}
\usage{
sales_filter(task, min.obs = 21, nfolds.cv.glmnet = 5, forecast_method = "lasso")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{task}{
Task which contains
}
  \item{min.obs}{
Minimum observation needed to make model for data filtration
}
  \item{nfolds.cv.glmnet}{
Number of k-fold in cvglmnet
}
  \item{forecast_method}{
lasso or lm
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (task, min.obs = 21, nfolds.cv.glmnet = 5, forecast_method = "lasso")
{
    start_time <- Sys.time()
    sales <- as.data.table(task$sales)
    sales$date <- date(sales$date)
    MP0 <- task$MP
    pricelog <- task$pricelog
    if (is.null(task$data_matrix)) {
        lager <- unique(sales$lagerID)
        filial <- unique(sales$filid)
        matrix <- as.data.frame(sales[, list(datefrom = date(min(date)),
            dateto = date(max(date))), by = c("lagerID", "filid")])
    }
    else {
        matrix <- task$data_matrix
    }
    sales <- as.data.frame(sales)
    MP <- MP0[, c("mainLagerId", "filialId", "marketProgramId",
        "documentId", "periodFrom", "periodTo")]
    MP <- MP[MP$marketProgramId \%in\% c(1, 376, 378, 451, 397,
        386, 568, 768, 749, 383, 384, 394, 530), ]
    MP$periodFrom <- date(MP$periodFrom)
    MP$periodTo <- date(MP$periodTo)
    MP <- as.data.table(MP)
    MP <- MP[, list(date = seq(periodFrom, periodTo, by = "1 day")),
        by = c("mainLagerId", "filialId", "marketProgramId",
            "documentId")]
    MP$isMP <- 1
    MP <- unique(MP, by = c("mainLagerId", "filialId", "marketProgramId",
        "date"))
    MP_long <- as.data.frame(MP)
    MP$marketProgramId <- paste("MP", MP$marketProgramId, sep = "")
    MP <- MP \%>\% spread(marketProgramId, documentId, fill = 0)
    MP[, 5:dim(MP)[2]][MP[, 5:dim(MP)[2]] > 1] <- 1
    MP <- as.data.frame(MP)
    matrix <- as.data.frame(matrix \%>\% rowwise() \%>\% do(data.frame(.[c("lagerID",
        "filid")], date = seq(.$datefrom, .$dateto, by = "1 day"))))
    if (is.null(MP)) {
        matrix$isMP <- 0
    }
    else {
        matrix <- left_join(matrix, MP, by = c(date = "date",
            lagerID = "mainLagerId", filid = "filialId"))
        matrix[is.na(matrix)] <- 0
    }
    matrix <- as.data.table(left_join(matrix, sales[, c("date",
        "lagerID", "filid", "sumOut_F", "kolvo_F", "kolvoStore")],
        by = c("date", "lagerID", "filid")))
    matrix <- matrix[, `:=`(kolvo_F, ifelse(kolvo_F < 0 | is.na(kolvo_F),
        0, kolvo_F))]
    matrix <- matrix[, `:=`(sumOut_F, ifelse(sumOut_F < 0 | is.na(sumOut_F),
        0, sumOut_F))]
    matrix$sumOut_F <- ifelse(matrix$sumOut_F > 0 & matrix$kolvo_F ==
        0, 0, matrix$sumOut_F)
    matrix$kolvo_F <- ifelse(matrix$kolvo_F > 0 & matrix$sumOut_F ==
        0, 0, matrix$kolvo_F)
    matrix <- matrix[, `:=`(kolvoStore, ifelse(kolvoStore < 0 |
        is.na(kolvoStore), 0, kolvoStore))]
    matrix <- as.data.frame(matrix)
    pricelog <- task$pricelog
    pricelog[pricelog$activeTo > Sys.Date(), ]$activeTo <- Sys.Date()
    pricelog$activeFrom <- date(pricelog$activeFrom)
    pricelog$activeTo <- date(pricelog$activeTo)
    pricelog <- as.data.table(pricelog)
    pricelog <- pricelog[, list(filialId, lagerId, price, date = seq(activeFrom,
        activeTo, by = "1 day")), by = 1:nrow(pricelog)]
    pricelog <- pricelog[, -1]
    names(pricelog) <- c("lagerId", "filialId", "price_log",
        "date")
    pricelog <- as.data.table(pricelog)
    pricelog <- as.data.frame(pricelog[, list(price_log = max(price_log)),
        by = c("lagerId", "filialId", "date")])
    matrix <- left_join(matrix, pricelog, by = c(lagerID = "lagerId",
        filid = "filialId", date = "date"))
    matrix <- as.data.table(matrix)
    matrix <- matrix[, `:=`(out_of_stock, ifelse(isMP == 0 &
        kolvoStore == 0 & kolvo_F == 0, 1, 0))]
    matrix <- matrix[, `:=`(frequency, length(kolvo_F)), by = c("lagerID",
        "filid")]
    matrix <- matrix[, `:=`(good_observ, frequency - sum(out_of_stock)),
        by = c("lagerID", "filid")]
    matrix <- matrix[, `:=`(min_kolvo, min(kolvo_F)), by = c("lagerID",
        "filid")]
    matrix <- matrix[, `:=`(max_kolvo, max(kolvo_F)), by = c("lagerID",
        "filid")]
    matrix <- matrix[order(date), `:=`(stock_id, rleid(out_of_stock)),
        by = c("lagerID", "filid")]
    matrix <- matrix[, `:=`(stock_length, length(date)), by = c("lagerID",
        "filid", "stock_id")]
    matrix <- matrix[, `:=`(count_unique, length(unique(kolvo_F))),
        by = c("lagerID", "filid")]
    matrix_mod_calculation <- matrix[matrix$good_observ > min.obs &
        matrix$count_unique > 3, ]
    matrix_no_mod_calculation <- matrix[(matrix$good_observ <=
        min.obs & matrix$good_observ > 2) | (matrix$good_observ >
        2 & matrix$count_unique < 4), ]
    small <- matrix[matrix$good_observ <= 2, ]
    matrix_mod_calculation$DAY <- as.factor(wday(matrix_mod_calculation$date))
    matrix_mod_calculation$MONTH <- as.factor(month(matrix_mod_calculation$date))
    matrix_mod_calculation$WEEK <- week(matrix_mod_calculation$date)
    matrix_mod_calculation$YEAR <- year(matrix_mod_calculation$date)
    matrix_mod_calculation$day_month <- day(matrix_mod_calculation$date)
    matrix_mod_calculation <- matrix_mod_calculation[order(date),
        `:=`(TREND, 1:length(date)), by = c("lagerID", "filid")]
    matrix_mod_calculation$NY <- ifelse(matrix_mod_calculation$day_month \%in\%
        c(25:31) & matrix_mod_calculation$MONTH == 12, 1, 0)
    matrix_mod_calculation$Easter <- ifelse(matrix_mod_calculation$date \%in\%
        append(append(append(append(append(append(append(append(seq(date("2012-04-09"),
            date("2012-04-15"), by = "day"), seq(date("2013-04-30"),
            date("2013-05-05"), by = "day")), seq(date("2014-04-14"),
            date("2014-04-20"), by = "day")), seq(date("2015-04-06"),
            date("2015-04-12"), by = "day")), seq(date("2016-04-25"),
            date("2016-05-01"), by = "day")), seq(date("2017-04-10"),
            date("2017-04-16"), by = "day")), seq(date("2018-04-02"),
            date("2018-04-08"), by = "day")), seq(date("2019-04-22"),
            date("2019-04-28"), by = "day")), seq(date("2020-04-13"),
            date("2020-04-19"), by = "day")), 1, 0)
    matrix_mod_calculation$StNick <- ifelse(matrix_mod_calculation$day_month \%in\%
        c(13:19) & matrix_mod_calculation$MONTH == 12, 1, 0)
    matrix_mod_calculation$StValentine <- ifelse(matrix_mod_calculation$day_month \%in\%
        c(8:14) & matrix_mod_calculation$MONTH == 2, 1, 0)
    matrix_mod_calculation$WomenDay <- ifelse(matrix_mod_calculation$day_month \%in\%
        c(2:8) & matrix_mod_calculation$MONTH == 3, 1, 0)
    matrix_mod_calculation$Christmas <- ifelse(matrix_mod_calculation$day_month \%in\%
        c(1:7) & matrix_mod_calculation$MONTH == 1, 1, 0)
    levels(matrix_mod_calculation$MONTH) <- c(levels(matrix_mod_calculation$MONTH),
        setdiff(c(1:12), levels(matrix_mod_calculation$MONTH)))
    levels(matrix_mod_calculation$DAY) <- c(levels(matrix_mod_calculation$DAY),
        setdiff(c(1:7), levels(matrix_mod_calculation$DAY)))
    matrix_mod_calculation$price <- ifelse(is.nan(matrix_mod_calculation$sumOut_F/matrix_mod_calculation$kolvo_F),
        matrix_mod_calculation$price_log, matrix_mod_calculation$sumOut_F/matrix_mod_calculation$kolvo_F)
    matrix_mod_calculation <- matrix_mod_calculation[, `:=`(price,
        (na.locf(price))), by = c("lagerID", "filid")]
    matrix_mod_calculation <- matrix_mod_calculation[order(date),
        `:=`(kolvo_F_lag, lag(kolvo_F, order_by = date, 1)),
        by = c("lagerID", "filid")]
    matrix_mod_calculation <- matrix_mod_calculation[order(date),
        `:=`(out_of_stock_lag, lag(out_of_stock, order_by = date,
            1)), by = c("lagerID", "filid")]
    matrix_mod_calculation <- matrix_mod_calculation[, `:=`(kolvo_F_lag,
        ifelse(is.na(kolvo_F_lag), kolvo_F, kolvo_F_lag))]
    matrix_mod_calculation <- matrix_mod_calculation[, `:=`(out_of_stock_lag,
        ifelse(is.na(out_of_stock_lag), out_of_stock, out_of_stock_lag))]
    matrix_mod_calculation <- matrix_mod_calculation[, `:=`(kolvo_F_lag,
        ifelse(out_of_stock_lag == 1, NA, kolvo_F_lag))]
    matrix_mod_calculation <- matrix_mod_calculation[, `:=`(kolvo_F_lag,
        na.locf(kolvo_F_lag)), by = c("lagerID", "filid")]
    matrix_mod_calculation <- as.data.frame(matrix_mod_calculation)
    out_of_stock_1 <- matrix_mod_calculation[matrix_mod_calculation$out_of_stock ==
        1 & matrix_mod_calculation$good_observ * 0.5 >= matrix_mod_calculation$stock_length,
        ]
    out_of_stock_1$outlier <- 0
    out_of_stock_mod_na <- matrix_mod_calculation[matrix_mod_calculation$out_of_stock ==
        1 & matrix_mod_calculation$good_observ * 0.5 < matrix_mod_calculation$stock_length,
        ]
    out_of_stock_mod_na <- out_of_stock_mod_na[, c("lagerID",
        "filid", "date", "isMP", "out_of_stock", "kolvoStore",
        "kolvo_F")]
    out_of_stock_no_mod_na <- matrix_no_mod_calculation[matrix_no_mod_calculation$out_of_stock ==
        1 & matrix_no_mod_calculation$good_observ * 0.5 < matrix_no_mod_calculation$stock_length,
        ]
    out_of_stock_no_mod_na <- out_of_stock_no_mod_na[, c("lagerID",
        "filid", "date", "isMP", "out_of_stock", "kolvoStore",
        "kolvo_F")]
    out_of_stock_na <- rbind(out_of_stock_mod_na, out_of_stock_no_mod_na)
    out_of_stock_na$isMPcor <- out_of_stock_na$isMP
    out_of_stock_na$outlier <- NA
    out_of_stock_na$out_of_stock <- NA
    out_of_stock_na$kolvo_filter <- NA
    out_of_stock_na <- out_of_stock_na[, c("lagerID", "filid",
        "date", "isMPcor", "outlier", "out_of_stock", "kolvoStore",
        "kolvo_F", "kolvo_filter")]
    matrix_no_mod_calculation <- matrix_no_mod_calculation[!(matrix_no_mod_calculation$out_of_stock ==
        1 & matrix_no_mod_calculation$good_observ * 0.5 < matrix_no_mod_calculation$stock_length),
        ]
    matrix_mod_calculation <- matrix_mod_calculation[matrix_mod_calculation$out_of_stock ==
        0, ]
    features_schedule <- c("TREND", "NY", "Easter", "StNick",
        "StValentine", "WomenDay", "Christmas", "price", "kolvo_F_lag")
    matrix_mod_calculation <- matrix_mod_calculation[, append(append(c("lagerID",
        "filid", "date", "YEAR", "DAY", "MONTH", "isMP", "out_of_stock",
        "kolvoStore", "kolvo_F"), features_schedule), names(matrix_mod_calculation[,
        grep("^MP.", names(matrix))]))]
    end_data_prepear <- Sys.time()
    start_lasso_modeling <- Sys.time()
    lasso_model <- dlply(matrix_mod_calculation, c("lagerID",
        "filid"), .progress = "time", function(x) {
        sales_matrix <- as.matrix(model.matrix(data = x[, -which(names(x) \%in\%
            c("lagerID", "filid", "date", "isMP", "out_of_stock",
                "kolvoStore", "YEAR"))], kolvo_F ~ .))
        lasso <- cv.glmnet(sales_matrix, x$kolvo_F, alpha = 1,
            nfolds = nfolds.cv.glmnet)
    })
    end_lasso_modeling <- Sys.time()
    lasso_coefficient <- data.frame(ldply(lasso_model, function(mod) {
        cvm <- mod[["cvm"]]
        cvm_improve <- round((1 - lag(cvm)/cvm) * 100, 2)
        cvm_improve[1] <- cvm_improve[2]
        s <- which.max(cvm_improve > -1)[1]
        lambda <- mod[["lambda"]][[s]]
        rownames_to_column(as.data.frame(as.matrix(coef.cv.glmnet(mod,
            s = lambda))), "Feature")
    }))
    lambda <- data.frame(ldply(lasso_model, function(mod) {
        cvm <- mod[["cvm"]]
        cvm_improve <- round((1 - lag(cvm)/cvm) * 100, 2)
        cvm_improve[1] <- cvm_improve[2]
        s <- which.max(cvm_improve > -1)[1]
        lambda <- mod[["lambda"]][[s]]
    }))
    colnames(lambda)[3] <- "lambda"
    significant_features <- lasso_coefficient[lasso_coefficient$X1 !=
        0, ]
    significant_features$Feature <- ifelse(significant_features$Feature ==
        "X.Intercept.", "1", (significant_features$Feature))
    end_feature_extract <- Sys.time()
    lm_model <- dlply(matrix_mod_calculation, c("lagerID", "filid"),
        .progress = "time", function(x) {
            sales_matrix <- as.data.frame(model.matrix(data = x[,
                -which(names(x) \%in\% c("date", "documentId"))],
                kolvo_F ~ . - 1))
            sales_matrix <- sales_matrix[, colSums(sales_matrix !=
                0) > 0]
            xt <- cbind(kolvo_F = x$kolvo_F, sales_matrix)
            m <- lm(paste("kolvo_F", paste(significant_features[significant_features$lagerID ==
                unique(x$lagerID) & significant_features$filid ==
                unique(x$filid), ]$Feature, collapse = "+"),
                sep = "~"), data = xt)
        })
    end_lm_modelling <- Sys.time()
    outliers <- ldply(lm_model, .progress = "time", function(a) {
        cook <- as.data.frame(transform(cooks.distance(a)))
    })
    names(outliers) <- c("lagerID", "filid", "cook")
    matrix_mod_calculation <- cbind(matrix_mod_calculation, cook = outliers$cook)
    matrix_mod_calculation <- as.data.table(matrix_mod_calculation)
    matrix_mod_calculation <- matrix_mod_calculation[, `:=`(cook,
        na.mean(cook)), by = c("lagerID", "filid")]
    matrix_mod_calculation <- matrix_mod_calculation[, `:=`(mean_cook,
        mean(cook)), by = c("lagerID", "filid")]
    matrix_mod_calculation <- matrix_mod_calculation[, `:=`(sd_cook,
        sd(cook)), by = c("lagerID", "filid")]
    matrix_mod_calculation$outlier <- ifelse(matrix_mod_calculation$cook >
        matrix_mod_calculation$mean_cook + 3 * matrix_mod_calculation$sd_cook &
        matrix_mod_calculation$isMP == 0, 1, 0)
    end_outlier_detection <- Sys.time()
    significant_MP <- significant_features[grep("MP", significant_features$Feature),
        c("lagerID", "filid", "Feature")]
    significant_MP$Feature <- as.numeric(substring(significant_MP$Feature,
        3))
    if (dim(significant_MP)[1] == 0) {
        MP0_cor <- NULL
    }
    else {
        MP0_cor <- inner_join(MP_long[, c("date", "mainLagerId",
            "filialId", "documentId", "marketProgramId", "isMP")],
            significant_MP, by = c(mainLagerId = "lagerID", filialId = "filid",
                marketProgramId = "Feature"))
        MP0_cor$marketProgramId <- paste("MP", MP0_cor$marketProgramId,
            sep = "")
        MP0_cor <- MP0_cor \%>\% spread(marketProgramId, documentId,
            fill = 0)
        MP0_cor[, 5:dim(MP0_cor)[2]][MP0_cor[, 5:dim(MP0_cor)[2]] >
            1] <- 1
        MP0_cor[, 5:dim(MP0_cor)[2]] <- NULL
        colnames(MP0_cor)[4] <- "isMPcor"
    }
    start_small <- Sys.time()
    small$isMPcor <- small$isMP
    small$outlier <- NA
    small$out_of_stock <- NA
    small$kolvo_filter <- NA
    small <- small[, c("lagerID", "filid", "date", "isMPcor",
        "outlier", "out_of_stock", "kolvoStore", "kolvo_F", "kolvo_filter")]
    if (dim(matrix_no_mod_calculation)[1] == 0) {
        result_no_mod = NULL
    }
    else {
        matrix_no_mod_calculation <- as.data.table(matrix_no_mod_calculation)
        matrix_no_mod_calculation <- matrix_no_mod_calculation[isMP ==
            0, `:=`(outlier, ifelse(mean(kolvo_F) + 3 * sd(kolvo_F) <
            kolvo_F, 1, 0)), by = c("lagerID", "filid")]
        matrix_no_mod_calculation <- matrix_no_mod_calculation[,
            `:=`(outlier, ifelse(is.na(outlier), 0, outlier))]
        matrix_no_mod_calculation <- matrix_no_mod_calculation[,
            `:=`(netSales, ifelse(isMP == 1 | out_of_stock ==
                1 | outlier == 1, NA, kolvo_F))]
        matrix_no_mod_calculation <- matrix_no_mod_calculation[,
            `:=`(forecast, ifelse(is.na(mean(netSales, na.rm = T)),
                0, na_ma(netSales, k = 2, weighting = "simple"))),
            by = c("lagerID", "filid")]
        matrix_no_mod_calculation$isMPcor <- matrix_no_mod_calculation$isMP
        matrix_no_mod_calculation <- matrix_no_mod_calculation[,
            `:=`(kolvo_filter, if (isMPcor == 1 | outlier ==
                1 | out_of_stock == 1) {
                forecast
            }
            else {
                kolvo_F
            }), by = c("lagerID", "filid", "date")]
        result_no_mod <- matrix_no_mod_calculation[, c("lagerID",
            "filid", "date", "isMPcor", "outlier", "out_of_stock",
            "kolvoStore", "kolvo_F", "kolvo_filter")]
    }
    result_no_mod <- rbind(result_no_mod, small, out_of_stock_na)
    end_small_data <- Sys.time()
    matrix_mod_calculation <- as.data.frame(matrix_mod_calculation)
    sales_cor_matrix <- rbind(matrix_mod_calculation[, names(matrix_mod_calculation) \%in\%
        names(out_of_stock_1)], out_of_stock_1[, names(out_of_stock_1) \%in\%
        names(matrix_mod_calculation)])
    if (is.null(MP0_cor)) {
        sales_cor_matrix$isMPcor <- 0
    }
    else {
        sales_cor_matrix <- left_join(sales_cor_matrix, MP0_cor,
            by = c(lagerID = "mainLagerId", filid = "filialId",
                date = "date"))
        sales_cor_matrix$isMPcor[is.na(sales_cor_matrix$isMPcor)] <- 0
    }
    sales_cor_matrix$price_origin <- sales_cor_matrix$price
    sales_cor_matrix <- as.data.table(sales_cor_matrix)
    PRICE_COR_MONTH <- sales_cor_matrix[isMPcor == 0, list(price_cor_month = mean(price)),
        by = c("filid", "lagerID", "MONTH", "YEAR")]
    PRICE_COR_ALL <- sales_cor_matrix[isMPcor == 0, list(price_cor_all = mean(price)),
        by = c("filid", "lagerID")]
    sales_cor_matrix <- left_join(sales_cor_matrix, PRICE_COR_MONTH[,
        c("lagerID", "filid", "MONTH", "YEAR", "price_cor_month")],
        by = c("lagerID", "filid", "MONTH", "YEAR"))
    sales_cor_matrix <- left_join(sales_cor_matrix, PRICE_COR_ALL[,
        c("lagerID", "filid", "price_cor_all")], by = c("lagerID",
        "filid"))
    sales_cor_matrix <- as.data.table(sales_cor_matrix)
    sales_cor_matrix <- sales_cor_matrix[, `:=`(price_cor, ifelse(is.na(price_cor_month),
        price_cor_all, price_cor_month))]
    sales_cor_matrix <- sales_cor_matrix[, `:=`(price, ifelse(isMPcor ==
        0, price, price_cor))]
    sales_cor_matrix <- sales_cor_matrix[, `:=`(isMPcor_lag,
        lag(isMPcor, order_by = date)), by = c("lagerID", "filid")]
    sales_cor_matrix[is.na(sales_cor_matrix$isMPcor_lag)]$isMPcor_lag <- 0
    sales_cor_matrix$kolvo_F_lag_origin <- sales_cor_matrix$kolvo_F
    sales_cor_matrix <- sales_cor_matrix[, `:=`(noMP_Sales, ifelse(isMPcor ==
        1 | out_of_stock == 1 | outlier == 1, NA, kolvo_F))]
    sales_cor_matrix <- sales_cor_matrix[order(date), `:=`(MA,
        na.ma(noMP_Sales, k = 3, weighting = "simple")), by = c("lagerID",
        "filid")]
    sales_cor_matrix <- sales_cor_matrix[order(date), `:=`(kolvo_F_lag_forecast,
        lag(MA, n = 1, order_by = date)), by = c("lagerID", "filid")]
    sales_cor_matrix <- sales_cor_matrix[, `:=`(kolvo_F_lag,
        ifelse(isMPcor == 1 & isMPcor_lag == 1, kolvo_F_lag_forecast,
            kolvo_F_lag))]
    sales_cor_matrix <- as.data.frame(sales_cor_matrix)
    end_data_prepear_forecast <- Sys.time()
    if (forecast_method == "lm") {
        sales_cor_matrix <- as.data.table(ddply(sales_cor_matrix,
            c("lagerID", "filid"), .progress = "time", function(x) {
                sales_matrix <- as.matrix(model.matrix(data = x[,
                  append(append(c("DAY", "MONTH", "kolvo_F"),
                    features_schedule), names(x[, grep("^MP.",
                    names(x))]))], kolvo_F ~ .))
                sales_matrix[, grep("^MP.", names(sales_matrix))] <- 0
                transform(x, forecast = round(predict(lm_model[[paste(unique(x$lagerID),
                  unique(x$filid), sep = ".")]], newdata = as.data.frame(sales_matrix)),
                  2))
            }))
    }
    else {
        if (forecast_method == "lasso") {
            sales_cor_matrix <- as.data.table(ddply(sales_cor_matrix,
                c("lagerID", "filid"), .progress = "time", function(x) {
                  sales_matrix <- as.matrix(model.matrix(data = x[,
                    append(append(c("DAY", "MONTH", "kolvo_F"),
                      features_schedule), names(x[, grep("^MP.",
                      names(x))]))], kolvo_F ~ .))
                  sales_matrix[, grep("^MP.", names(sales_matrix))] <- 0
                  transform(x, forecast_lasso = round(predict(lasso_model[[paste(unique(x$lagerID),
                    unique(x$filid), sep = ".")]], s = lambda[lambda$lagerID ==
                    unique(x$lagerID) & lambda$filid == unique(x$filid),
                    ]$lambda, newx = as.matrix(sales_matrix)),
                    2))
                }))
        }
        else {
            error = "need forecast method"
        }
    }
    names(sales_cor_matrix)[dim(sales_cor_matrix)[2]] <- "forecast"
    sales_cor_matrix$forecast <- ifelse(sales_cor_matrix$forecast <
        0, 0, sales_cor_matrix$forecast)
    sales_cor_matrix$out_of_stock <- ifelse(sales_cor_matrix$kolvo_F <
        sales_cor_matrix$forecast & sales_cor_matrix$kolvoStore <
        sales_cor_matrix$forecast & sales_cor_matrix$isMPcor ==
        0, 1, 0)
    sales_cor_matrix$kolvo_filter <- ifelse(sales_cor_matrix$isMPcor ==
        1 | sales_cor_matrix$outlier == 1 | sales_cor_matrix$out_of_stock ==
        1, sales_cor_matrix$forecast, sales_cor_matrix$kolvo_F)
    result_mod <- sales_cor_matrix[, c("lagerID", "filid", "date",
        "isMPcor", "outlier", "out_of_stock", "kolvoStore", "kolvo_F",
        "kolvo_filter")]
    result <- rbind(result_mod, result_no_mod)
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
