round_away_0 = function (x, digits = 0, trailing_zeros = FALSE)
{
    rounded_vals <- sign(x) * round(abs(x) + 1e-15, digits)
    if (trailing_zeros) {
        rounded_vals[!is.na(rounded_vals)] <- formatC(rounded_vals[!is.na(rounded_vals)],
            digits, format = "f")
        neg_to_change <- paste0("-0.", paste0(rep(0, digits),
            collapse = ""))
        if (any(rounded_vals == neg_to_change, na.rm = TRUE))
            rounded_vals[rounded_vals == neg_to_change] <- substr(neg_to_change,
                2, nchar(neg_to_change))
    }
    rounded_vals
}


round_if_numeric = function (x, digits = 0, trailing_zeros = FALSE)
{
    if (is.numeric(x))
        round_away_0(x, digits = digits, trailing_zeros = trailing_zeros)
    else x
}

stat_paste = function (stat1, stat2 = NULL, stat3 = NULL, digits = 0, trailing_zeros = TRUE,
    bound_char = c("(", "[", "{", "|"), sep = ", ", na_str_out = "---")
{
    bound_char <- match.arg(bound_char)
    end_bound_char <- switch(bound_char, `(` = ")", `[` = "]",
        `{` = "}", `|` = "|")
    stat1_pasted_obj <- ifelse(is.na(stat1), na_str_out, as.character(round_if_numeric(stat1,
        digits = digits, trailing_zeros = trailing_zeros)))
    if (is.null(stat2)) {
        pasted_output <- stat1_pasted_obj
    }
    else {
        stat2_pasted_obj <- ifelse(is.na(stat2), na_str_out,
            as.character(round_if_numeric(stat2, digits = digits,
                trailing_zeros = trailing_zeros)))
        if (is.null(stat3)) {
            pasted_output <- ifelse(is.na(stat1) & is.na(stat2),
                na_str_out, paste0(stat1_pasted_obj, " ", bound_char,
                  stat2_pasted_obj, end_bound_char))
        }
        else {
            stat3_pasted_obj <- ifelse(is.na(stat3), na_str_out,
                as.character(round_if_numeric(stat3, digits = digits,
                  trailing_zeros = trailing_zeros)))
            pasted_output <- ifelse(is.na(stat1) & is.na(stat2) &
                is.na(stat3), na_str_out, paste0(stat1_pasted_obj,
                " ", bound_char, stat2_pasted_obj, sep, stat3_pasted_obj,
                end_bound_char))
        }
    }
    pasted_output
}

clean_pvalues = function (pvalues, digits = 3, bold = FALSE, italic = FALSE,
    background = NULL, sig_alpha = 0.05, missing_char = "---", trailing_zeros = TRUE)
{
    lower_cutoff = 10^(-digits)
    missing_p = which(is.na(pvalues))
    below_cutoff_p = which(pvalues < lower_cutoff)
    sig_p = which(pvalues < sig_alpha)
    if (trailing_zeros)
        pvalues_new = round_away_0(pvalues, trailing_zeros = TRUE,
            digits = digits) else pvalues_new =
        as.character(round_away_0(pvalues, trailing_zeros = FALSE, digits))
    pvalues_new[missing_p] = missing_char
    pvalues_new[below_cutoff_p] = paste0("<", lower_cutoff)

    pvalues_new[sig_p] = kableExtra::cell_spec(pvalues_new[sig_p],
                                               bold = bold, italic = italic,
                                               background = background, escape = FALSE)

    pvalues_new
}
