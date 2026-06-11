     hdatedit(*ymd) datfmt(*ymd)

     fWMCHP     if   f  110        disk                                          Input file
     fAFTWMCHPO o    e             printer oflind(*in71)                         Query report

      ** Constants
      *************************************************
     dcnTITLE          c                   const('Query Conversion for')         Title
     dcnTYPE           c                   const('Old File')                     File (new/old)

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

      ** CLAIM HISTORY MASTER FILE
      *************************************************
     iWMCHP     ns
     i                             P    1    2 0CHCO#                            COMPANY NUMBER
     i                             P    3    4 0CHFND                            FUND NUMBER
     i                             P    5    6 0CHFYR                            FUND CENTURY AN
     i                             P    7   10 0CHCASE                           CASE NUMBER
     i                                 11   16 0CHEFDT                           EFFECTIVE DATE
     i                                 17   17  CHOCST                           OPEN/ CLOSE STA
     i                                 18   77  CHEXPL                           EXPLANATION
     i                                 78   78  CHSTAT                           STATUS
     i                                 79   88  CHUSER                           USER
     i                                 89   98  CHPGM                            PROGRAM
     i                                 99  104 0CHTIME                           TIME
     i                                105  110 0CHDT                             DATE MAINTAINED

      ** Program entry
      *************************************************
     c     *entry        plist
     c                   parm                    pmFILE           10             File name

      ** Mainline processing
      *************************************************
     c                   read      wmchp                                  LR
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
     csr                 eval      ptQFIL=%trim('*LIBL  ')+'/'+pmFILE              :
     csr                 endsr                                                   <-+

      ** Finish up routine
      *************************************************
     csr   $Finish       begsr                                                   >-+
     csr                 eval      chCO#=*loval                                    :
     csr                 eval      chFND=*loval                                    :
     csr                 eval      chFYR=*loval                                    :
     csr                 exsr      $Write_Hd                                       :
     csr                 exsr      $Write_Bk1                                      :
     csr                 exsr      $Write_Tl                                       :
     csr                 endsr                                                   <-+

      ** Process a record routine
      *************************************************
     csr   $Process      begsr                                                   >---+
     csr                 eval      xxCOUNT=xxCOUNT+1                                 :
     csr                 if        xxCOUNT=1                                     >-+ :
     csr                 eval      xxCO#=chCO#                                     : :
     csr                 eval      xxFND=chFND                                     : :
     csr                 eval      xxFYR=chFYR                                     : :
     csr                 eval      xxOCST=chOCST                                   : :
     csr                 endif                                                   <-+ :
     csr                 if        xxCO#<>chCO# or xxFND<>chFND                  >-+ :
     c                             or xxFYR<>chFYR or xxOCST<>chOCST               : :
     csr                 exsr      $Write_Hd                                       : :
     csr                 exsr      $Write_Bk1                                      : :
     csr                 eval      xxCO#=chCO#                                     : :
     csr                 eval      xxFND=chFND                                     : :
     csr                 eval      xxFYR=chFYR                                     : :
     csr                 eval      xxOCST=chOCST                                   : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxCOUNT1=xxCOUNT1+1                               :
     csr                 eval      xxCOUNT2=xxCOUNT2+1                               :
     csr                 eval      xxCOUNT3=xxCOUNT3+1                               :
     csr                 endsr                                                   <---+

      ** Write break 1 line
      *************************************************
     csr   $Write_Bk1    begsr                                                   >-----+
     csr                 if        xxCO#<>chCO# or xxFND<>chFND or xxFYR<>chFYR  >-+   :
     csr                 eval      *in61=*on                                       :   :
     csr                 eval      xxPRINT=*zero                                   :   :
     csr                 endif                                                   <-+   :
     csr                 write     PRTBK1                                              :
     csr                 eval      xxPRINT=xxPRINT+1                                   :
     csr                 eval      xxCOUNT1=*zero                                      :
     csr                 if        *in61=*on                                     >---+ :
     csr                 write     PRTBK2                                            : :
     csr                 eval      *in61=*off                                        : :
     csr                 eval      xxCOUNT2=*zero                                    : :
     csr                 if        xxCO#<>chCO# or xxFND<>chFND                  >-+ : :
     csr                 write     PRTBK3                                          : : :
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
