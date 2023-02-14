get_targeting <- function(id, timeframe = "LAST_30_DAYS") {

    url <- "https://www.facebook.com/api/graphql/"

    heads_up <- httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0",
                                  Accept = "*/*",
                                  `Accept-Language` = 'en-US,en;q=0.5',
                                  `X-FB-Friendly-Name` = "AdLibraryPageAudienceTabQuery",
                                  `X-FB-LSD`= "AVrNiQCSUnA",
                                  `Alt-Used`= "www.facebook.com",
                                  `Sec-Fetch-Dest`= "empty",
                                  `Sec-Fetch-Mode`= "cors",
                                  `Sec-Fetch-Site`= "same-origin",
                                  # `Accept-Encoding` = "gzip, deflate, br",
                                  `Content-Type` = "application/x-www-form-urlencoded",
                                  Connection = "keep-alive"
    )


    if(timeframe == "LAST_30_DAYS"){

        # audienceTimeframe <- "%7B%22audienceTimeframe%22%3A%22LAST_30_DAYS%22%2C%22"
        da_body <- glue::glue("av=0&__user=0&__a=1&__dyn=7xeUmxa3-Q8zo5ObwKBWobVo9E4a2i5U4e1FxebzEdF8ixy7EiwvoWdwJwCwAwgU2lxS6Ehwem0nCqbwgE3awbG78b87C1xwEwgolzUO0n2US2G3i1ywa-2l0Fwwwi831wnFokwyx2cw8WfK6E5i3e4U3mxOu2S2W2K7o725U4q0HUkyE9E11EbodEGdw46wbLwiU8U6C2-&__csr=&__req=m&__hs=19237.BP%3ADEFAULT.2.0.0.0.0&dpr=1&__ccg=EXCELLENT&__rev=1006139712&__s=ll61s1%3Axn89ey%3Admpplc&__hsi=7138774996758193009&__comet_req=0&lsd=AVrNiQCSYrc&jazoest=2981&__spin_r=1006139712&__spin_b=trunk&__spin_t=1662125577&__jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryPageAudienceTabQuery&variables=%7B%22audienceTimeframe%22%3A%22LAST_30_DAYS%22%2C%22viewAllPageID%22%3A%22{id}%22%7D&server_timestamps=true&doc_id=4756112137823411") %>% as.character()

    } else if (timeframe == "LAST_7_DAYS"){

        # audienceTimeframe <- "%7B%22"
        da_body <- glue::glue("av=0&__user=0&__a=1&__dyn=7xeUmxa3-Q8zo5ObwKBWobVo9E4a2i5U4e1FxebzEdF8aUuxa1ZzES2S2q2i13w9m7oqx60Vo1upEK12wcG0KEswIwuo662y11xmfz81sbzoaEd86a0HU9k2C2218wc61uBxi2a48O0zE-Uqwl8cUjwdq79UbobEaUtws8nwhE2LxiawCw46wJwSyES0gq0K-1bwzwqobU&__csr=&__req=f&__hs=19245.BP%3ADEFAULT.2.0.0.0.0&dpr=1&__ccg=EXCELLENT&__rev=1006179750&__s=njkc5w%3A6o847a%3A9gcoa8&__hsi=7141736891942848978&__comet_req=0&lsd=AVrbeuAiHJg&jazoest=21000&__spin_r=1006179750&__spin_b=trunk&__spin_t=1662815197&__jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryPageAudienceTabQuery&variables=%7B%22audienceTimeframe%22%3A%22LAST_7_DAYS%22%2C%22viewAllPageID%22%3A%22{id}%22%7D&server_timestamps=true&doc_id=4756112137823411") %>% as.character()

    } else if (timeframe == "LAST_90_DAYS"){

        da_body <- glue::glue("av=0&__user=0&__a=1&__dyn=7xeUmxa3-Q8zo5ObwKBWobVo9E4a2i5U4e1FxebzEdF8aUuxa1ZzES2S2q2i13w9m7oqx60Vo1upEK12wcG0KEswIwuo662y11xmfz81sbzoaEd86a0HU9k2C2218wc61uBxi2a48O3u1mzXxG1kwPxe3C0D8sDwJwKwHxS1Mxu16wa-58G2q0gq2S3qazo11E2XU4K2e1FwLw8O2i&__csr=&__req=h&__hs=19301.BP%3ADEFAULT.2.0.0.0.0&dpr=1&__ccg=EXCELLENT&__rev=1006553893&__s=20shv5%3A62a2bj%3A6goj90&__hsi=7162612241770415577&__comet_req=0&lsd=AVohzhTn68E&jazoest=2965&__spin_r=1006553893&__spin_b=trunk&__spin_t=1667675618&__jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryPageAudienceTabQuery&variables=%7B%22audienceTimeframe%22%3A%22LAST_90_DAYS%22%2C%22viewAllPageID%22%3A%22{id}%22%7D&server_timestamps=true&doc_id=4756112137823411") %>% as.character()

        url <- "https://www.facebook.com/api/graphql/"

        heads_up <- httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0",
                                      Accept = "*/*",
                                      `Accept-Language` = 'en-US,en;q=0.5',
                                      `X-FB-Friendly-Name` = "AdLibraryPageAudienceTabQuery",
                                      `X-FB-LSD`= "AVrNiQCSUnA",
                                      `Alt-Used`= "www.facebook.com",
                                      `Sec-Fetch-Dest`= "empty",
                                      `Sec-Fetch-Mode`= "cors",
                                      `Sec-Fetch-Site`= "same-origin",
                                      # `Accept-Encoding` = "gzip, deflate, br",
                                      `Content-Type` = "application/x-www-form-urlencoded",
                                      Connection = "keep-alive"
        )

    }





    posted = httr::POST(url, heads_up, body = da_body)

    contentwise <- httr::content(posted)

    rate_limit <- str_detect(as.character(contentwise), "Rate limit exceeded")
    if(rate_limit){
        # stop(as.character(contentwise))
        beepr::beep(20)
        readline("Please change VPN")
    }



    out_raw <- contentwise %>%
        rvest::html_nodes("body") %>%
        rvest::html_nodes("p") %>%
        as.character() %>% str_remove_all("</p>|<p>") %>%
        jsonlite::fromJSON()  %>%
        purrr::pluck("data") %>%
        purrr::pluck("page") %>%
        purrr::pluck("ad_library_page_targeting_insight")


    summary_dat <- out_raw %>%
        purrr::pluck("ad_library_page_targeting_summary") %>%
        dplyr::bind_rows()

    if(nrow(summary_dat) > 1){

        summary_dat <- summary_dat %>%
            dplyr::slice(which(summary_dat$detailed_spend$currency == summary_dat$main_currency)) %>%
            dplyr::select(-detailed_spend)

    }

    targeting_details_raw <- out_raw[!(names(out_raw) %in% c("ad_library_page_targeting_summary", "ad_library_page_has_siep_ads"))]

    # names(targeting_details_raw)

    res <- targeting_details_raw %>%
        purrr::discard(purrr::is_empty) %>%
        purrr::imap_dfr(~{.x %>% dplyr::mutate(type = .y %>% stringr::str_remove("ad_library_page_targeting_"))}) %>%
        dplyr::bind_cols(summary_dat) %>%
        dplyr::mutate(internal_id = id)

    return(res)

}




library(httr)
library(tidyverse)

tstamp <- Sys.time()

scraper <- function(.x, timeframe = "7") {

    # print(paste0(.x$page_name,": ", round(which(internal_page_ids$page_id == .x$page_id)/nrow(internal_page_ids)*100, 2)))

    yo <- get_targeting(.x$page_id, timeframe = paste0("LAST_",timeframe,"_DAYS")) %>%
        mutate(tstamp = tstamp)

    if(nrow(yo)!=0){
        path <- paste0(timeframe,"/",.x$page_id, ".rds")
        # if(file.exists(path)){
        #   ol <- read_rds(path)
        #
        #   saveRDS(yo %>% bind_rows(ol), file = path)
        # } else {

        saveRDS(yo, file = path)
        # }
    }

    # print(nrow(yo))
    # })

}

# scraper <- possibly(scraper, otherwise = NULL, quiet = F)


### save seperately
yo <- lab_dat %>% #count(cntry, sort  =T) %>%
    # filter(!(page_id %in% already_there)) %>%
    # filter(cntry == "GB") %>%
    # slice(1:10) %>%
    filter(!(page_id %in% these_are_here_too$internal_id)) %>%
    split(1:nrow(.)) %>%
    map_dfr_progress(~{scraper(.x, "7")})


last30days <- dir("30", full.names = T) %>%
    map_dfr_progress(~{
        readRDS(.x) %>% mutate(path = .x)
    })

last7days <- dir("7", full.names = T) %>%
    map_dfr_progress(~{
        readRDS(.x) %>% mutate(path = .x)
    })

these_are_here <- last30days %>%
    distinct(internal_id)

these_are_here_too <- last7days %>%
    distinct(internal_id)


election_dat7 <- last7days %>%
    left_join(lab_dat %>% rename(internal_id = page_id))  %>%
    mutate(total_spend_formatted = parse_number(total_spend_formatted)) %>%
    mutate(election = ifelse(election == "Lombardy", "Lombardia", election))

saveRDS(election_dat7, "data/election_dat7.rds")

election_dat30 <- last30days %>%
    left_join(lab_dat %>% rename(internal_id = page_id))  %>%
    mutate(total_spend_formatted = parse_number(total_spend_formatted)) %>%
    mutate(election = ifelse(election == "Lombardy", "Lombardia", election))

saveRDS(election_dat30, "data/election_dat30.rds")

election_dat30 %>%
    group_by(coalition) %>%
    summarize(total_num_ads = n()) %>%
    drop_na() %>%
    mutate(total_num_ads = scales::comma(total_num_ads)) %>%
    pivot_wider(names_from = coalition, values_from = total_num_ads) %>%
    mutate(type = "Total Number of Advertisers") %>%
bind_rows(
    election_dat30 %>%
        distinct(internal_id, .keep_all = T) %>%
        group_by(coalition) %>%
        arrange(desc(total_spend_formatted)) %>%
        slice(1:3) %>%
        mutate(total_spend_formatted = scales::comma(total_spend_formatted)) %>%
        mutate(lab = paste0(page_name, " ($", total_spend_formatted, ")")) %>%
        select(coalition, lab) %>%
        drop_na() %>%
        summarize(lab = paste0("<br>", 1:3, ". ", lab, collapse = "")) %>%
        pivot_wider(names_from = coalition, values_from = lab) %>%
        mutate(type = "Top Advertiser")
)  %>%

    bind_rows(
        election_dat30 %>%
            distinct(internal_id, .keep_all = T) %>%
            group_by(coalition) %>%
            summarize(total_num_ads = sum(total_num_ads)) %>%
            drop_na() %>%
            mutate(total_num_ads = scales::comma(total_num_ads)) %>%
            pivot_wider(names_from = coalition, values_from = total_num_ads) %>%
            mutate(type = "Total Number of Ads")
    )%>%
    bind_rows(
        election_dat30 %>%
            distinct(internal_id, .keep_all = T) %>%
            group_by(coalition) %>%
            summarize(total_spend_formatted = sum(total_spend_formatted)) %>%
            mutate(total_spend_formatted = scales::comma(total_spend_formatted)) %>%
            drop_na() %>%
            pivot_wider(names_from = coalition, values_from = total_spend_formatted) %>%
            mutate(type = "Total Spend ($)")
    )
