     hdatedit(*ymd) datfmt(*ymd)

     fWDCXP     if   f  254        disk                                          Input file
     fAFTWDCXPO o    e             printer oflind(*in71)                         Query report

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

      ** CANCELLATION DETAIL FILE
      *************************************************
     iWDCXP     ns
     i                             P    1    2 0CXCO#                            COMPANY NUMBER
     i                             P    3    4 0CXFND                            FUND NUMBER
     i                             P    5    6 0CXFYR                            FUND CENTURY AN
     i                             P    7    9 0CXEMP#                           EMPLOYER NUMBER
     i                             P   10   11 0CXDIV                            DIVISION
     i                             P   12   14 0CXREQ#                           SEQUENCE NUMBER
     i                                 15   20 0CXCXDT                           CANCEL DATE
     i                                 21   26 0CXCDAT                           DATE CANCELLATI
     i                             P   27   29 0CXAGT#                           AGENT NUMBER
     i                                 30   31  CXRSN                            CANCEL REASON
     i                                 32   91  CXCOM1                           COMMENT 1
     i                                 92  151  CXCOM2                           COMMENT 2
     i                                152  152  CXCCRT                           CANCEL CERT
     i                                153  154  CXCDRT                           CANCEL DART COD
     i                                155  155  CXREIN                           REINSTATED FLAG
     i                                156  161 0CXREDT                           REINSTATEMENT D
     i                                162  167 0CXRDAT                           DATE REINSTATEM
     i                                168  168  CXRCRT                           REINSTATEMENT C
     i                                169  170  CXRDRT                           REINSTATEMENT D
     i                                171  171  CXSTAT                           STATUS
     i                                172  181  CXUSER                           USER
     i                                182  191  CXPGM                            PROGRAM
     i                                192  197 0CXTIME                           TIME
     i                                198  203 0CXDT                             DATE MAINTAINED
     i                                204  204  CXREVL                           NCCI REINSTATE
     i                             P  205  209 2CXAMTD                           AMOUNT DUE
     i                             P  210  214 2CXFEED                           FEES DUE
     i                                215  254  CXCARR                           CARRIER NAME

      ** Program entry
      *************************************************
     c     *entry        plist
     c                   parm                    pmFILE           10             File name

      ** Mainline processing
      *************************************************
     c                   read      wdcxp                                  LR
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
     csr                 eval      *in61=*off                                      :
     csr                 eval      ptPAGE=*zero                                    :
     csr                 eval      xxCOUNT=*zero                                   :
     csr                 eval      xxPRINT=*zero                                   :
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
     csr                 eval      cxCO#=*loval                                    :
     csr                 eval      cxFND=*loval                                    :
     csr                 exsr      $Write_Hd                                       :
     csr                 exsr      $Write_Bk1                                      :
     csr                 exsr      $Write_Tl                                       :
     csr                 endsr                                                   <-+

      ** Process a record routine
      *************************************************
     csr   $Process      begsr                                                   >---+
     csr                 eval      xxCOUNT=xxCOUNT+1                                 :
     csr                 if        xxCOUNT=1                                     >-+ :
     csr                 eval      xxCO#=cxCO#                                     : :
     csr                 eval      xxFND=cxFND                                     : :
     csr                 eval      xxFYR=cxFYR                                     : :
     csr                 endif                                                   <-+ :
     csr                 if        xxCO#<>cxCO# or xxFND<>cxFND or xxFYR<>cxFYR  >-+ :
     csr                 exsr      $Write_Hd                                       : :
     csr                 exsr      $Write_Bk1                                      : :
     csr                 eval      xxCO#=cxCO#                                     : :
     csr                 eval      xxFND=cxFND                                     : :
     csr                 eval      xxFYR=cxFYR                                     : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxCOUNT1=xxCOUNT1+1                               :
     csr                 eval      xxCOUNT2=xxCOUNT2+1                               :
     csr                 endsr                                                   <---+

      ** Write break 1 line
      *************************************************
     csr   $Write_Bk1    begsr                                                   >---+
     csr                 if        xxCO#<>cxCO# or xxFND<>cxFND                  >-+ :
     csr                 eval      *in61=*on                                       : :
     csr                 eval      xxPRINT=*zero                                   : :
     csr                 endif                                                   <-+ :
     csr                 write     PRTBK1                                            :
     csr                 eval      xxPRINT=xxPRINT+1                                 :
     csr                 eval      xxCOUNT1=*zero                                    :
     csr                 if        *in61=*on                                     >-+ :
     csr                 write     PRTBK2                                          : :
     csr                 eval      *in61=*off                                      : :
     csr                 eval      xxCOUNT2=*zero                                  : :
     csr                 endif                                                   <-+ :
     csr                 endsr                                                   <---+

      ** Write a print header
      *************************************************
     csr   $Write_Hd     begsr                                                   >---+
     csr                 if        *in71=*on                                     >-+ :
     csr                 eval      *in71=*off                                      : :
     csr                 eval      ptPAGE=ptPAGE+1                                 : :
     csr                 write     PrtHdr                                          : :
     csr                 endif                                                   <-+ :
     csr                 endsr                                                   <---+

      ** Write total line
      *************************************************
     csr   $Write_Tl     begsr                                                   >-+
     csr                 write     PRTTOT                                          :
     csr                 endsr                                                   <-+
