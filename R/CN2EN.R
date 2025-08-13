#' @title Turn Chinese virus names to English
#' @description
#' Use pinyin package to translate Chinese Virus names to English to match GISAID sequences.
#' @param TB_list a table list.
#' @param parallel a logical value to indicate whether to use parallel processing. Default is TRUE.
#' @import progressr pinyin stringr future.apply progressr

CN2EN <- function(TB_list, parallel = TRUE) {
    handlers(global = TRUE)

    result <- progressr::with_progress({
        p <- progressr::progressor(steps = length(TB_list))
        if (parallel) {
            plan(multisession)
        } else {
            plan(sequential)
        }

        future.apply::future_lapply(seq_along(TB_list), function(i) {
            tryCatch({
                TB <- TB_list[[i]]
                V_names <- TB[, 1, drop = TRUE]
                split_list <- strsplit(V_names, "/")
                candi_V <- lapply(split_list, function(x) x[2])

                contains_CN <- str_detect(candi_V, "[\u4e00-\u9fa5]")
                result <- unlist(
                    sapply(seq_along(candi_V), function(i) {
                        if (contains_CN[i]) {
                            convert_chinese(candi_V[[i]])
                        } else {
                            candi_V[[i]]
                        }
                    })
                )

                TB$Virus <- unlist(
                    lapply(seq_along(result), function(j) {
                        sp_vec <- split_list[[j]]
                        paste(sp_vec[1], result[j], sp_vec[3], sp_vec[4], sep = "/")
                    })
                )

                p()
                return(TB)
            },
            error = function(e){
                message(sprintf("Error in item %d: %s", i, e$message))
                p()
                return()})
            return(result)
        })
    })
    return(result)
}


convert_chinese <- function(CN_string) {
    provinces <- c("bei_jing", "tian_jin", "he_bei", "shan_xi", "nei_meng_gu", "liao_ning",
                   "ji_lin", "hei_long_jiang", "shang_hai", "jiang_su", "zhe_jiang", "an_hui",
                   "fu_jian", "jiang_xi", "shan_dong", "he_nan", "hu_bei", "hu_nan",
                   "guang_dong", "guang_xi", "hai_nan", "chong_qing", "si_chuan", "gui_zhou",
                   "yun_nan", "xi_zang", "shan_xi", "gan_su", "qing_hai", "ning_xia",
                   "xin_jiang", "xiang_gang", "ao_men")

    penv <- pinyin::pydic(dic = "pinyin2", method = "toneless")
    Full_py <- pinyin::py(CN_string, sep = "_", dic = penv)
    matched_region <- str_extract(Full_py, paste(provinces, collapse = "|"))

    if (is.na(matched_region)) {
        message("no matchine place")
        message(Full_py)
        return(Full_py)
    } else {
        Province <- tools::toTitleCase(gsub("_", "", matched_region, fixed = TRUE))
        if(nchar(matched_region) == nchar(Full_py)){
            result <- Province
        } else {
            District <- tools::toTitleCase(gsub("_", "", sub(matched_region, "", Full_py)))
            result <- paste0(Province, "-", District)
        }
        return(result)
    }
}
