     hdatedit(*ymd) datfmt(*ymd)

     fWMEAP     if   f  104        disk                                          Input file
     fAFTWMEAPO o    e             printer oflind(*in71)                         Query report

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

      ** EMPLOYER ACCOUNTS RECEIVABLE
      *************************************************
     iWMEAP     ns
     i                             P    1    2 0EACO#                            COMPANY NUMBER
     i                             P    3    4 0EAFND                            FUND NUMBER
     i                             P    5    6 0EAFYR                            FUND CENTURY AN
     i                             P    7    9 0EAEMP#                           EMPLOYER NUMBER
     i                             P   10   11 0EADIV                            DIVISION
     i                                 12   13  EAARCD                           AR TYPE CODE
     i                                 14   14  EAARTY                           AR TYPE
     i                                 15   44  EADESC                           AR DESCRIPTION
     i                             P   45   49 2EAAMT                            AR AMOUNT
     i                                 50   55 0EATRDT                           TRANSACTION DAT
     i                                 56   61 0EAPODT                           POSTING DATE
     i                                 62   62  EAEDIT                           EDIT FLAG
     i                                 63   63  EASTAT                           STATUS
     i                                 64   73  EAUSER                           USER
     i                                 74   83  EAPGM                            PROGRAM
     i                                 84   89 0EATIME                           TIME
     i                                 90   95 0EADT                             DATE MAINTAINED
     i                             P   96  100 0EABUL1                           BATCH UPDATE LO
     i                             P  101  104 0EACHK#                           CHECK NUMBER

      ** Program entry
      *************************************************
     c     *entry        plist
     c                   parm                    pmFILE           10             File name

      ** Mainline processing
      *************************************************
     c                   read      wmeap                                  LR
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
     csr                 eval      eaCO#=*loval                                    :
     csr                 eval      eaFND=*loval                                    :
     csr                 eval      eaFYR=*loval                                    :
     csr                 exsr      $Write_Hd                                       :
     csr                 exsr      $Write_Bk1                                      :
     csr                 exsr      $Write_Tl                                       :
     csr                 endsr                                                   <-+

      ** Process a record routine
      *************************************************
     csr   $Process      begsr                                                   >---+
     csr                 eval      xxCOUNT=xxCOUNT+1                                 :
     csr                 eval      xxAMT=xxAMT+eaAMT                                 :
     csr                 if        xxCOUNT=1                                     >-+ :
     csr                 eval      xxCO#=eaCO#                                     : :
     csr                 eval      xxFND=eaFND                                     : :
     csr                 eval      xxFYR=eaFYR                                     : :
     csr                 eval      xxARCD=eaARCD                                   : :
     csr                 endif                                                   <-+ :
     csr                 if        xxCO#<>eaCO# or xxFND<>eaFND                  >-+ :
     c                             or xxFYR<>eaFYR or xxARCD<>eaARCD               : :
     csr                 exsr      $Write_Hd                                       : :
     csr                 exsr      $Write_Bk1                                      : :
     csr                 eval      xxCO#=eaCO#                                     : :
     csr                 eval      xxFND=eaFND                                     : :
     csr                 eval      xxFYR=eaFYR                                     : :
     csr                 eval      xxARCD=eaARCD                                   : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxCOUNT1=xxCOUNT1+1                               :
     csr                 eval      xxAMT1=xxAMT1+eaAMT                               :
     csr                 eval      xxCOUNT2=xxCOUNT2+1                               :
     csr                 eval      xxAMT2=xxAMT2+eaAMT                               :
     csr                 eval      xxCOUNT3=xxCOUNT3+1                               :
     csr                 eval      xxAMT3=xxAMT3+eaAMT                               :
     csr                 endsr                                                   <---+

      ** Write break 1 line
      *************************************************
     csr   $Write_Bk1    begsr                                                   >-----+
     csr                 if        xxCO#<>eaCO# or xxFND<>eaFND or xxFYR<>eaFYR  >-+   :
     csr                 eval      *in61=*on                                       :   :
     csr                 eval      xxPRINT=*zero                                   :   :
     csr                 endif                                                   <-+   :
     csr                 write     PRTBK1                                              :
     csr                 eval      xxPRINT=xxPRINT+1                                   :
     csr                 eval      xxAMT1=*zero                                        :
     csr                 eval      xxCOUNT1=*zero                                      :
     csr                 if        *in61=*on                                     >---+ :
     csr                 write     PRTBK2                                            : :
     csr                 eval      *in61=*off                                        : :
     csr                 eval      xxAMT2=*zero                                      : :
     csr                 eval      xxCOUNT2=*zero                                    : :
     csr                 if        xxCO#<>eaCO# or xxFND<>eaFND                  >-+ : :
     csr                 write     PRTBK3                                          : : :
     csr                 eval      xxAMT3=*zero                                    : : :
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
