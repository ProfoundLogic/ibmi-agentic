     hdatedit(*ymd) datfmt(*ymd)

     fWMEPP     if   e             disk    rename(pep01:DataBase)                Input file
     fAFTWMEPP2Oo    e             printer oflind(*in71)                         Query report

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
     csr                 eval      epCO#=*loval                                    :
     csr                 eval      epFND=*loval                                    :
     csr                 exsr      $Write_Hd                                       :
     csr                 exsr      $Write_Bk1                                      :
     csr                 exsr      $Write_Tl                                       :
     csr                 endsr                                                   <-+

      ** Process a record routine
      *************************************************
     csr   $Process      begsr                                                   >---+
     csr                 eval      xxCOUNT=xxCOUNT+1                                 :
     csr                 eval      xxSAPR=xxSAPR+epSAPR                              :
     csr                 eval      xxEAPR=xxEAPR+epEAPR                              :
     csr                 eval      xxEMPR=xxEMPR+epEMPR                              :
     csr                 eval      xxAAPR=xxAAPR+epAAPR                              :
     csr                 eval      xxAMPR=xxAMPR+epAMPR                              :
     csr                 if        xxCOUNT=1                                     >-+ :
     csr                 eval      xxCO#=epCO#                                     : :
     csr                 eval      xxFND=epFND                                     : :
     csr                 eval      xxFYR=epFYR                                     : :
     csr                 endif                                                   <-+ :
     csr                 if        xxCO#<>epCO# or xxFND<>epFND or xxFYR<>epFYR  >-+ :
     csr                 exsr      $Write_Hd                                       : :
     csr                 exsr      $Write_Bk1                                      : :
     csr                 eval      xxCO#=epCO#                                     : :
     csr                 eval      xxFND=epFND                                     : :
     csr                 eval      xxFYR=epFYR                                     : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxCOUNT1=xxCOUNT1+1                               :
     csr                 eval      xxSAPR1=xxSAPR1+epSAPR                            :
     csr                 eval      xxEAPR1=xxEAPR1+epEAPR                            :
     csr                 eval      xxEMPR1=xxEMPR1+epEMPR                            :
     csr                 eval      xxAAPR1=xxAAPR1+epAAPR                            :
     csr                 eval      xxAMPR1=xxAMPR1+epAMPR                            :
     csr                 eval      xxCOUNT2=xxCOUNT2+1                               :
     csr                 eval      xxSAPR2=xxSAPR2+epSAPR                            :
     csr                 eval      xxEAPR2=xxEAPR2+epEAPR                            :
     csr                 eval      xxEMPR2=xxEMPR2+epEMPR                            :
     csr                 eval      xxAAPR2=xxAAPR2+epAAPR                            :
     csr                 eval      xxAMPR2=xxAMPR2+epAMPR                            :
     csr                 endsr                                                   <---+

      ** Write break 1 line
      *************************************************
     csr   $Write_Bk1    begsr                                                   >---+
     csr                 if        xxCO#<>epCO# or xxFND<>epFND                  >-+ :
     csr                 eval      *in61=*on                                       : :
     csr                 eval      xxPRINT=*zero                                   : :
     csr                 endif                                                   <-+ :
     csr                 write     PRTBK1                                            :
     csr                 eval      xxPRINT=xxPRINT+1                                 :
     csr                 eval      xxSAPR1=*zero                                     :
     csr                 eval      xxEAPR1=*zero                                     :
     csr                 eval      xxEMPR1=*zero                                     :
     csr                 eval      xxAAPR1=*zero                                     :
     csr                 eval      xxAMPR1=*zero                                     :
     csr                 eval      xxCOUNT1=*zero                                    :
     csr                 if        *in61=*on                                     >-+ :
     csr                 write     PRTBK2                                          : :
     csr                 eval      *in61=*off                                      : :
     csr                 eval      xxSAPR2=*zero                                   : :
     csr                 eval      xxEAPR2=*zero                                   : :
     csr                 eval      xxEMPR2=*zero                                   : :
     csr                 eval      xxAAPR2=*zero                                   : :
     csr                 eval      xxAMPR2=*zero                                   : :
     csr                 eval      xxCOUNT2=*zero                                  : :
     csr                 endif                                                   <-+ :
     csr                 endsr                                                   <---+

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
