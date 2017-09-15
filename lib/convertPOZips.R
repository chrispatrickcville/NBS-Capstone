convertPOZips <- function(df=NULL, zip_col=NULL, input_check=NULL) {
  
  # Converts zip codes from P.O. Box Zips to the 
  # surrounding zip code. Used for allowing P.O.
  # Box zips to be included in diseas mapping
  # (these zips are not included in the data source
  # used for choroplethZip)
  
  # Also checks to see if input zip codes will be
  # converted in mapping
  
  # Source for identifying the surrounding area
  # zip codes:
  # http://virginia.hometownlocator.com/zip-codes/ (enter zip, scoll down to map)
  
  # Load zip.regions data
  data(zip.regions)
  
  # pairings:
  zip_code_pairings <- data.frame(ORIGINALZIP=character(),
                                  REVISEDZIP=character())
  
  pair_1 <- c("20104","20166")
  pair_2 <- c("20108","20110")
  pair_3 <- c("20113","20111")
  pair_4 <- c("20131","20132")
  pair_5 <- c("20131","20132") 
  pair_6 <- c("20140","20115") 
  pair_7 <- c("20142","20141") 
  pair_8 <- c("20146","20166")
  pair_9 <- c("20153","20151")
  pair_10 <- c("20156","20155")
  pair_11 <- c("20163","20166")
  pair_12 <- c("20167","20164")
  pair_13 <- c("20168","20169")
  pair_14 <- c("20177","20175")
  pair_15 <- c("20182","20181")
  pair_16 <- c("20185","20184")
  pair_17 <- c("20189","20006")
  pair_18 <- c("20195","20190")
  pair_19 <- c("22034","22031")
  pair_20 <- c("22036","22031")
  pair_21 <- c("22040","22046")
  pair_22 <- c("22106","22101")
  pair_23 <- c("22107","22102")
  pair_24 <- c("22161","22151")
  pair_25 <- c("22183","22180")
  pair_26 <- c("22194","22191")
  pair_27 <- c("22195","22193")     
  pair_28 <- c("22199","22079")
  pair_29 <- c("22210","22201")
  pair_30 <- c("22219","22209")
  pair_31 <- c("22240","22203")
  pair_32 <- c("22244","22202")
  pair_33 <- c("22313","22314")
  pair_34 <- c("22402","22405")
  pair_35 <- c("22403","22406")
  pair_36 <- c("22430","22554")
  pair_37 <- c("22472","22572")
  pair_38 <- c("22481","22485")
  pair_39 <- c("22507","22503")
  pair_40 <- c("22517","22503")
  pair_41 <- c("22524","22520")
  pair_42 <- c("22552","22427")
  pair_43 <- c("22555","22554")
  pair_44 <- c("22565","22580")
  pair_45 <- c("22581","22520")
  pair_46 <- c("22649","22645")
  pair_47 <- c("22803","22802")
  pair_48 <- c("22905","22903")
  pair_49 <- c("22906","22901")
  pair_50 <- c("22908","22903")
  pair_51 <- c("22909","22911")
  pair_52 <- c("22924","22903")
  pair_53 <- c("23031","23092")
  pair_54 <- c("23090","23188")
  pair_55 <- c("23183","23061")
  pair_56 <- c("23184","23072")
  pair_57 <- c("23186","23185")
  pair_58 <- c("23218","23219")
  pair_59 <- c("23232","23220")
  pair_60 <- c("23241","23219")
  pair_61 <- c("23242","23238")
  pair_62 <- c("23260","23220")
  pair_63 <- c("23279","23222") 
  pair_64 <- c("23288","23229")
  pair_65 <- c("23290","23219")
  pair_66 <- c("23298","23219")
  pair_67 <- c("23326","23320")
  pair_68 <- c("23327","23320")
  pair_69 <- c("23328","23322")
  pair_70 <- c("23431","23430")  
  pair_71 <- c("23439","23434")
  pair_72 <- c("23450","23452")
  pair_73 <- c("23458","23451")
  pair_74 <- c("23466","23462")
  pair_75 <- c("23501","23504")
  pair_76 <- c("23514","23510")
  pair_77 <- c("23515","23551")
  pair_78 <- c("23541","23510")  
  pair_79 <- c("23609","23602")
  pair_80 <- c("23668","23651")
  pair_81 <- c("23670","23661")
  pair_82 <- c("23694","23690")
  pair_83 <- c("23705","23704")
  pair_84 <- c("23804","23803")
  pair_85 <- c("23939","24522")
  pair_86 <- c("24001","24011")
  pair_87 <- c("24010","24011")
  pair_88 <- c("24029","24016")
  pair_89 <- c("24033","24016")
  pair_90 <- c("24114","24112")
  pair_91 <- c("24126","24301")
  pair_92 <- c("24129","24141")
  pair_93 <- c("24157","24012")
  pair_94 <- c("24402","24401")
  pair_95 <- c("24535","24520")
  pair_96 <- c("24506","24501")
  pair_97 <- c("27969","27935") # North Carolina
  
  for (i in 1:97) {
    var <- paste0("pair_", i)
    zip_code_pairings <- rbind(zip_code_pairings, eval(parse(text=var)), stringsAsFactors=FALSE)
  }
  
  names(zip_code_pairings) <- c("ORIGINALZIP", "REVISEDZIP")
  
  if (!is.null(input_check)) {
    
    # Print out error message if any zip codes appear in neither zip_code_pairings or zip.regions
    while (!all(input_check %in% c(zip_code_pairings$ORIGINALZIP, zip.regions$region))) {
      cat(addLineBreaks("\nERROR: You've entered at least one value for zip_code_vals that is not a recognized zip code. Please reenter the zip codes you wish to map (separated by commas).\n"))
      input_check <- trim(unlist(strsplit(readline(prompt = "> "), ",")))
    }  
    
    # Print out warning message if any zip codes will be changed in the output map
    if (any(input_check %in% zip_code_pairings$ORIGINALZIP)) {
      for (z in input_check[input_check %in% zip_code_pairings$ORIGINALZIP]) {
        alt = zip_code_pairings$REVISEDZIP[zip_code_pairings$ORIGINALZIP==z]
        cat(addLineBreaks(sprintf("\nWARNING: The zip code you entered, %s, is not a zip code that can be mapped 'as is.' However, this zip code is either within or adjacent to the zip code %s, which can be mapped, so any MOTHERZIP of %s will be changed to %s for mapping.\n",
                                  z, alt, z, alt)))
        input_check[input_check==z] <- alt
      }
    }
    
    # Update values of zip_code_vals in environment
    zip_code_vals <<- input_check
    
  } else {
    
    # For any items of zip_col that are found in zip_code_pairings, replace
    # the value in the df with the value in zip_code_pairings$REVISEDZIP
    
    if (nrow(df) > 1) {
      
      df[, zip_col] <- unlist(sapply(df[, zip_col], 
                                     function(x) ifelse(x %in% zip_code_pairings$ORIGINALZIP, 
                                                        zip_code_pairings$REVISEDZIP[zip_code_pairings$ORIGINALZIP==x], x)))
    
    } else {
      
      df[1, zip_col] <- ifelse(df[1, zip_col] %in% zip_code_pairings$ORIGINALZIP, 
                               zip_code_pairings$REVISEDZIP[zip_code_pairings$ORIGINALZIP==df[1, zip_col]], 
                               df[1, zip_col])

    }
    
    return(df)
    
  }
    
}