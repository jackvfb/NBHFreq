myFunc <- function(data, calibration=NULL) {
  # List names of all required packages here to guarantee they will be
  # installed when others try to use your function, ex:
  # packageList <- c('seewave', 'signal')
  packageList <- c()
  for(p in packageList) {
    if(!require(p, character.only = TRUE)) {
      install.packages(p)
      require(p, character.only = TRUE)
    }
  }
  # prepping output
  result <- list()
  # Do same stuff for each channel
  for(chan in 1:ncol(data$wave)) {
    # create storage for this channels outputs
    thisResult <- list()
    ##### DO STUFF AFTER HERE #######
    thisWave <- data$wave[, chan]

    ####### SAMPLE CALIBRATION CODE, MODIFY AS NEEDED ###
    # Calibration applies to power spec (20*log10 space)
    thisSpec <- spec(thisWave, f=data$sr, wl=512, norm=FALSE, correction='amplitude', plot=FALSE)
    # `spec` has problems with high samplerates producing NA frequencies (integer overflow)
    # so we recreate the frequency vector in a more stable way
    freq <- seq(from=0, by = thisSpec[2,1] - thisSpec[1,1], length.out = nrow(thisSpec))
    dB <- 20*log10(thisSpec[,2])
    # This chunk searches for the calibration function added with `addCalibration`
    if(!is.null(calibration)) {
      #DO CA
      if(is.function(calibration)) {
        calFun <- calibration
      } else if(is.character(calibration)) {
        calFun <- findCalibration(calibration)
      }
      # Function takes in frequnecy in kHz and returns dB correction
      dB <- dB + calFun(freq * 1e3)
    }
    # dB is now adjusted for calibration, use as you like:
    calibratedMax <- freq[which.max(dB)]
    #### END CALIBRATION SECTION #####

    # Store results you want output in `thisResult`:
    # wavMax <- max(thisWave)
    # thisResult$wavMax <- wavMax
    #### STORE THIS CHANNEL ####
    thisResult <- list(test = 1, test2 = 2)
    result[[chan]] <- thisResult
  }
  # Combine results for each channel into a single dataframe
  # Theres no need to label each channel, this handles in earlier steps within PAMpal
  result <- bind_rows(result)
  result
}
x <- myFunc(testClick)
