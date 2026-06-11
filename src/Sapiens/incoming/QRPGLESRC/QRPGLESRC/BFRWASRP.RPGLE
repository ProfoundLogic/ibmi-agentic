     hdatedit(*ymd) datfmt(*ymd)

     fWASRP     if   f   77        disk                                          Input file
     fAFTWASRPO o    e             printer oflind(*in71)                         Query report

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

      ** EMPLOYER PAYROLL FILE
      *************************************************
     iWASRP     ns
     i                             P    1    2 0SRCO#                            COMPANY NUMBER
     i                             P    3    4 0SRFND                            FUND NUMBER
     i                             P    5    6 0SRFYR                            FUND CENTURY AN
     i                             P    7    9 0SREMP#                           EMPLOYER NUMBER
     i                             P   10   11 0SRDIV                            DIVISION
     i                             P   12   14 0SRCLAS                           CLASS CODE
     i                                 15   20 0SREFDT                           EFFECTIVE DATE
     i                             P   21   23 0SRRPMY                           REPORTING MONTH
     i                             P   24   29 2SRAMTP                           PAYROLL AMOUNT
     i                             P   30   35 2SRORGP                           ORIGINAL PAYROL
     i                                 36   36  SRTYFL                           ENTRY TYPE
     i                                 37   37  SRSC                             SUB-CONTRACTOR
     i                                 38   38  SRSTAT                           STATUS
     i                                 39   48  SRUSER                           USER
     i                                 49   58  SRPGM                            PROGRAM
     i                                 59   64 0SRTIME                           TIME
     i                                 65   70 0SRDT                             DATE MAINTAINED
     i                                 71   76 0SRCADT                           CANCELLATION DA
     i                                 77   77  SRSPEC                           SPECIAL DES

      ** Program entry
      *************************************************
     c     *entry        plist
     c                   parm                    pmFILE           10             File name

      ** Mainline processing
      *************************************************
     c                   read      wasrp                                  LR
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
     csr                 eval      srCO#=*loval                                    :
     csr                 eval      srFND=*loval                                    :
     csr                 exsr      $Write_Hd                                       :
     csr                 exsr      $Write_Bk1                                      :
     csr                 exsr      $Write_Tl                                       :
     csr                 endsr                                                   <-+

      ** Process a record routine
      *************************************************
     csr   $Process      begsr                                                   >---+
     csr                 eval      xxCOUNT=xxCOUNT+1                                 :
     csr                 eval      xxAMTP=xxAMTP+srAMTP                              :
     csr                 eval      xxORGP=xxORGP+srORGP                              :
     csr                 if        xxCOUNT=1                                     >-+ :
     csr                 eval      xxCO#=srCO#                                     : :
     csr                 eval      xxFND=srFND                                     : :
     csr                 eval      xxFYR=srFYR                                     : :
     csr                 endif                                                   <-+ :
     csr                 if        xxCO#<>srCO# or xxFND<>srFND or xxFYR<>srFYR  >-+ :
     csr                 exsr      $Write_Hd                                       : :
     csr                 exsr      $Write_Bk1                                      : :
     csr                 eval      xxCO#=srCO#                                     : :
     csr                 eval      xxFND=srFND                                     : :
     csr                 eval      xxFYR=srFYR                                     : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxCOUNT1=xxCOUNT1+1                               :
     csr                 eval      xxAMTP1=xxAMTP1+srAMTP                            :
     csr                 eval      xxORGP1=xxORGP1+srORGP                            :
     csr                 eval      xxCOUNT2=xxCOUNT2+1                               :
     csr                 eval      xxAMTP2=xxAMTP2+srAMTP                            :
     csr                 eval      xxORGP2=xxORGP2+srORGP                            :
     csr                 endsr                                                   <---+

      ** Write break 1 line
      *************************************************
     csr   $Write_Bk1    begsr                                                   >---+
     csr                 if        xxCO#<>srCO# or xxFND<>srFND                  >-+ :
     csr                 eval      *in61=*on                                       : :
     csr                 eval      xxPRINT=*zero                                   : :
     csr                 endif                                                   <-+ :
     csr                 write     PRTBK1                                            :
     csr                 eval      xxPRINT=xxPRINT+1                                 :
     csr                 eval      xxAMTP1=*zero                                     :
     csr                 eval      xxORGP1=*zero                                     :
     csr                 eval      xxCOUNT1=*zero                                    :
     csr                 if        *in61=*on                                     >-+ :
     csr                 write     PRTBK2                                          : :
     csr                 eval      *in61=*off                                      : :
     csr                 eval      xxAMTP2=*zero                                   : :
     csr                 eval      xxORGP2=*zero                                   : :
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
