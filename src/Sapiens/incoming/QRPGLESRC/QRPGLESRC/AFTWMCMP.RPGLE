     hdatedit(*ymd) datfmt(*ymd)

     fWMCMP     if   e             disk    rename(pcm01:DataBase)                Input file
     fAFTWMCMPO o    e             printer oflind(*in71)                         Query report

      ** Constants
      *************************************************
     dcnTITLE          c                   const('Query Conversion for')         Title
     dcnTYPE           c                   const('New File')                     File (new/old)

      ** Initialization work area
      *************************************************
     dWork_Inz         ds                  inz                                   Initialization
     d  xxCCYYMMDD                    8s 0                                         Date is CCYYMMDD
     d    xxCCYY                      4a   overlay(xxCCYYMMDD:1)                     Century, year
     d    xxMM                        2a   overlay(xxCCYYMMDD:5)                     Month
     d    xxDD                        2a   overlay(xxCCYYMMDD:7)                     Day
     d  xxEDATE                      10a                                           Edited date
     d  xxHRMNSC                      6s 0                                         Time is HHMMSS
     d    xxHR                        2a   overlay(xxHRMNSC:1)                       Hour
     d    xxMN                        2a   overlay(xxHRMNSC:3)                       Minute
     d    xxSC                        2a   overlay(xxHRMNSC:5)                       Second
     d  xxETIME                       8a                                           Edited time

      ** Work/Storage area
      *************************************************
     dWork_Stor        ds                  inz                                   Work
     d  IX                            5p 0                                         Index
     d  xxCOUNT                       9p 0                                         Record counter
     d  xxPRINT                       9p 0                                         Print counter

      ** Program entry
      *************************************************
     c     *entry        plist
     c                   parm                    pmFILE           10             File name
     c                   parm                    pmLIBRARY        10             Library

     c     *like         define    cmRR01        tmRAM            +2
     c     *like         define    cmAR01        tmPTD            +2

      ** Mainline processing
      *************************************************
     c                   read      DataBase                               LR
     c                   if        *inLR=*on                                     >-+
     c                   exsr      $Finish                                         :
     c                   else                                                    >-+
     c                   exsr      $Process                                        :
     c                   endif                                                   <-+

      ** Program initialization routine
      *************************************************
     csr   *inzsr        begsr                                                   >-+
     csr                 move      *date         xxCCYYMMDD                        :
     csr                 time                    xxHRMNSC                          :
     csr                 eval      xxEDATE=xxMM+'/'+xxDD+'/'+xxCCYY                :
     csr                 eval      xxETIME=xxHR+':'+xxMN+':'+xxSC                  :
     csr                 eval      *in71=*on                                       :
     csr                 eval      ptPAGE=*zero                                    :
     csr                 eval      xxCOUNT=*zero                                   :
     csr                 eval      ptRDAT=xxEDATE                                  :
     csr                 eval      ptRTIM=xxETIME                                  :
     csr                 eval      ptTITL=cnTITLE+' '+cnTYPE+' '+pmFILE            :
     csr   ' '           checkr    ptTITL        IX                                :
     csr                 eval      IX=(%size(ptTITL)-IX)/2                         :
     csr   ' '           cat       ptTITL:IX     ptTITL                            :
     csr                 eval      ptQFIL=%trim(pmLIBRARY)+'/'+pmFILE              :
     csr                 endsr                                                   <-+

      ** Finish up routine
      *************************************************
     csr   $Finish       begsr                                                   >-+
     csr                 eval      cmCO#=*loval                                    :
     csr                 eval      cmFND=*loval                                    :
     csr                 eval      cmFYR=*loval                                    :
     csr                 exsr      $Write_Hd                                       :
     csr                 exsr      $Write_Bk1                                      :
     csr                 exsr      $Write_Tl                                       :
     csr                 endsr                                                   <-+

      ** Process a record routine
      *************************************************
     csr   $Process      begsr                                                   >---+
     csr                 eval      tmRAM=cmRR01+cmRR02+cmRR03+cmRR04+cmRR05+         :
     c                             cmRR06+cmRR07+cmRR08+cmRR09+cmRR10                :
     csr                 eval      tmPTD=cmAR01+cmAR02+cmAR03+cmAR04+cmAR05+         :
     c                             cmAR06+cmAR07+cmAR08+cmAR09+cmAR10                :
     csr                 eval      xxCOUNT=xxCOUNT+1                                 :
     csr                 eval      xxRAM=xxRAM+tmRAM                                 :
     csr                 eval      xxPTD=xxPTD+tmPTD                                 :
     csr                 eval      xxINC=xxINC+tmRAM+tmPTD                           :
     csr                 if        xxCOUNT=1                                     >-+ :
     csr                 eval      xxCO#=cmCO#                                     : :
     csr                 eval      xxFND=cmFND                                     : :
     csr                 eval      xxFYR=cmFYR                                     : :
     csr                 eval      xxSTAT=cmSTAT                                   : :
     csr                 endif                                                   <-+ :
     csr                 if        xxCO#<>cmCO# or xxFND<>cmFND                  >-+ :
     c                             or xxFYR<>cmFYR or xxSTAT<>cmSTAT               : :
     csr                 exsr      $Write_Hd                                       : :
     csr                 exsr      $Write_Bk1                                      : :
     csr                 eval      xxCO#=cmCO#                                     : :
     csr                 eval      xxFND=cmFND                                     : :
     csr                 eval      xxFYR=cmFYR                                     : :
     csr                 eval      xxSTAT=cmSTAT                                   : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxCOUNT1=xxCOUNT1+1                               :
     csr                 eval      xxRAM1=xxRAM1+tmRAM                               :
     csr                 eval      xxPTD1=xxPTD1+tmPTD                               :
     csr                 eval      xxINC1=xxINC1+tmRAM+tmPTD                         :
     csr                 eval      xxCOUNT2=xxCOUNT2+1                               :
     csr                 eval      xxRAM2=xxRAM2+tmRAM                               :
     csr                 eval      xxPTD2=xxPTD2+tmPTD                               :
     csr                 eval      xxINC2=xxINC2+tmRAM+tmPTD                         :
     csr                 eval      xxCOUNT3=xxCOUNT3+1                               :
     csr                 eval      xxRAM3=xxRAM3+tmRAM                               :
     csr                 eval      xxPTD3=xxPTD3+tmPTD                               :
     csr                 eval      xxINC3=xxINC3+tmRAM+tmPTD                         :
     csr                 endsr                                                   <---+

      ** Write break 1 line
      *************************************************
     csr   $Write_Bk1    begsr                                                   >-----+
     csr                 if        xxCO#<>cmCO# or xxFND<>cmFND or xxFYR<>cmFYR  >-+   :
     csr                 eval      *in61=*on                                       :   :
     csr                 eval      xxPRINT=*zero                                   :   :
     csr                 endif                                                   <-+   :
     csr                 write     PRTBK1                                              :
     csr                 eval      xxPRINT=xxPRINT+1                                   :
     csr                 eval      xxRAM1=*zero                                        :
     csr                 eval      xxPTD1=*zero                                        :
     csr                 eval      xxINC1=*zero                                        :
     csr                 eval      xxCOUNT1=*zero                                      :
     csr                 if        *in61=*on                                     >---+ :
     csr                 write     PRTBK2                                            : :
     csr                 eval      *in61=*off                                        : :
     csr                 eval      xxRAM2=*zero                                      : :
     csr                 eval      xxPTD2=*zero                                      : :
     csr                 eval      xxINC2=*zero                                      : :
     csr                 eval      xxCOUNT2=*zero                                    : :
     csr                 if        xxCO#<>cmCO# or xxFND<>cmFND                  >-+ : :
     csr                 write     PRTBK3                                          : : :
     csr                 eval      xxRAM3=*zero                                    : : :
     csr                 eval      xxPTD3=*zero                                    : : :
     csr                 eval      xxINC3=*zero                                    : : :
     csr                 eval      xxCOUNT3=*zero                                  : : :
     csr                 endif                                                   <-+ : :
     csr                 write     PRTBLK                                            : :
     csr                 endif                                                   <---+ :
     csr                 endsr                                                   <-----+

      ** Write a print header
      *************************************************
     csr   $Write_Hd     begsr                                                   >---+
     csr                 if        *in71=*on                                     >-+ :
     csr                 eval      *in71=*off                                      : :
     csr                 eval      ptPAGE=ptPAGE+1                                 : :
     csr                 write     PRTHDR                                          : :
     csr                 endif                                                   <-+ :
     csr                 endsr                                                   <---+

      ** Write total line
      *************************************************
     csr   $Write_Tl     begsr                                                   >-+
     csr                 write     PRTTOT                                          :
     csr                 endsr                                                   <-+
