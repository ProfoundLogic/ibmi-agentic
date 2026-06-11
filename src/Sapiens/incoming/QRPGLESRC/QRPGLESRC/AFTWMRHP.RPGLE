     hdatedit(*ymd) datfmt(*ymd)

     fWMRHP     if   e             disk    rename(prh01:DataBase)                Input file
     fAFTWMRHPO o    e             printer oflind(*in71)                         Query report

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
     csr                 eval      rhCO#=*loval                                    :
     csr                 eval      rhFND=*loval                                    :
     csr                 exsr      $Write_Hd                                       :
     csr                 exsr      $Write_Bk1                                      :
     csr                 exsr      $Write_Tl                                       :
     csr                 endsr                                                   <-+

      ** Process a record routine
      *************************************************
     csr   $Process      begsr                                                   >---+
     csr                 eval      xxCOUNT=xxCOUNT+1                                 :
     csr                 eval      xxSU01=xxSU01+rhSU01                              :
     csr                 eval      xxSU02=xxSU02+rhSU02                              :
     csr                 eval      xxSU03=xxSU03+rhSU03                              :
     csr                 eval      xxSU04=xxSU04+rhSU04                              :
     csr                 eval      xxSU05=xxSU05+rhSU05                              :
     csr                 eval      xxSU06=xxSU06+rhSU06                              :
     csr                 eval      xxSU07=xxSU07+rhSU07                              :
     csr                 eval      xxSU08=xxSU08+rhSU08                              :
     csr                 eval      xxSU09=xxSU09+rhSU09                              :
     csr                 eval      xxSU10=xxSU10+rhSU10                              :
     csr                 if        xxCOUNT=1                                     >-+ :
     csr                 eval      xxCO#=rhCO#                                     : :
     csr                 eval      xxFND=rhFND                                     : :
     csr                 eval      xxFYR=rhFYR                                     : :
     csr                 endif                                                   <-+ :
     csr                 if        xxCO#<>rhCO# or xxFND<>rhFND or xxFYR<>rhFYR  >-+ :
     csr                 exsr      $Write_Hd                                       : :
     csr                 exsr      $Write_Bk1                                      : :
     csr                 eval      xxCO#=rhCO#                                     : :
     csr                 eval      xxFND=rhFND                                     : :
     csr                 eval      xxFYR=rhFYR                                     : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxCOUNT1=xxCOUNT1+1                               :
     csr                 eval      xxSU011=xxSU011+rhSU01                            :
     csr                 eval      xxSU021=xxSU021+rhSU02                            :
     csr                 eval      xxSU031=xxSU031+rhSU03                            :
     csr                 eval      xxSU041=xxSU041+rhSU04                            :
     csr                 eval      xxSU051=xxSU051+rhSU05                            :
     csr                 eval      xxSU061=xxSU061+rhSU06                            :
     csr                 eval      xxSU071=xxSU071+rhSU07                            :
     csr                 eval      xxSU081=xxSU081+rhSU08                            :
     csr                 eval      xxSU091=xxSU091+rhSU09                            :
     csr                 eval      xxSU101=xxSU101+rhSU10                            :
     csr                 eval      xxCOUNT2=xxCOUNT2+1                               :
     csr                 eval      xxSU012=xxSU012+rhSU01                            :
     csr                 eval      xxSU022=xxSU022+rhSU02                            :
     csr                 eval      xxSU032=xxSU032+rhSU03                            :
     csr                 eval      xxSU042=xxSU042+rhSU04                            :
     csr                 eval      xxSU052=xxSU052+rhSU05                            :
     csr                 eval      xxSU062=xxSU062+rhSU06                            :
     csr                 eval      xxSU072=xxSU072+rhSU07                            :
     csr                 eval      xxSU082=xxSU082+rhSU08                            :
     csr                 eval      xxSU092=xxSU092+rhSU09                            :
     csr                 eval      xxSU102=xxSU102+rhSU10                            :
     csr                 endsr                                                   <---+

      ** Write break 1 line
      *************************************************
     csr   $Write_Bk1    begsr                                                   >---+
     csr                 if        xxCO#<>rhCO# or xxFND<>rhFND                  >-+ :
     csr                 eval      *in61=*on                                       : :
     csr                 eval      xxPRINT=*zero                                   : :
     csr                 endif                                                   <-+ :
     csr                 write     PRTBK1                                            :
     csr                 eval      xxPRINT=xxPRINT+1                                 :
     csr                 eval      xxSU011=*zero                                     :
     csr                 eval      xxSU021=*zero                                     :
     csr                 eval      xxSU031=*zero                                     :
     csr                 eval      xxSU041=*zero                                     :
     csr                 eval      xxSU051=*zero                                     :
     csr                 eval      xxSU061=*zero                                     :
     csr                 eval      xxSU071=*zero                                     :
     csr                 eval      xxSU081=*zero                                     :
     csr                 eval      xxSU091=*zero                                     :
     csr                 eval      xxSU101=*zero                                     :
     csr                 eval      xxCOUNT1=*zero                                    :
     csr                 if        *in61=*on                                     >-+ :
     csr                 write     PRTBK2                                          : :
     csr                 eval      *in61=*off                                      : :
     csr                 eval      xxSU012=*zero                                   : :
     csr                 eval      xxSU022=*zero                                   : :
     csr                 eval      xxSU032=*zero                                   : :
     csr                 eval      xxSU042=*zero                                   : :
     csr                 eval      xxSU052=*zero                                   : :
     csr                 eval      xxSU062=*zero                                   : :
     csr                 eval      xxSU072=*zero                                   : :
     csr                 eval      xxSU082=*zero                                   : :
     csr                 eval      xxSU092=*zero                                   : :
     csr                 eval      xxSU102=*zero                                   : :
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
