     hdatedit(*ymd) datfmt(*ymd)

     fWDDDP     if   f   96        disk                                          Input file
     fAFTWDDDPO o    e             printer oflind(*in71)                         Query report

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

      ** DEDUCTIBLE DETAIL FILE
      *************************************************
     iWDDDP     ns
     i                             P    1    2 0DDCO#                            COMPANY NUMBER
     i                             P    3    4 0DDFND                            FUND NUMBER
     i                             P    5    6 0DDBFYR                           BILLING FUND YE
     i                             P    7    9 0DDEMP#                           EMPLOYER NUMBER
     i                             P   10   11 0DDDIV                            DIVISION
     i                             P   12   13 0DDFYR                            CLAIM FUND YEAR
     i                             P   14   17 0DDCASE                           CASE NUMBER
     i                                 18   23 0DDPODT                           INVOICE DATE
     i                             P   24   28 2DDAMTB                           AMOUNT BILLED
     i                             P   29   33 2DDAMTR                           AMOUNT RECOVERE
     i                             P   34   38 2DDAMTP                           AMOUNT PENDING
     i                                 39   40  DDRECD                           RESERVE CODE
     i                             P   41   42 0DDSORT                           RESERVE HIERARC
     i                             P   43   47 0DDBAT#                           BATCH #
     i                                 48   48  DDSTAT                           STATUS
     i                                 49   58  DDUSER                           USER
     i                                 59   68  DDPGM                            PROGRAM
     i                                 69   74 0DDTIME                           TIME
     i                                 75   80 0DDDT                             DATE
     i                                 81   90  DDIUSR                           RECORD CREATED
     i                                 91   96 0DDIDT                            DATE RECORD CRE

      ** Program entry
      *************************************************
     c     *entry        plist
     c                   parm                    pmFILE           10             File name

      ** Mainline processing
      *************************************************
     c                   read      wdddp                                  LR
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
     csr                 eval      ddCO#=*loval                                    :
     csr                 eval      ddFND=*loval                                    :
     csr                 exsr      $Write_Hd                                       :
     csr                 exsr      $Write_Bk1                                      :
     csr                 exsr      $Write_Tl                                       :
     csr                 endsr                                                   <-+

      ** Process a record routine
      *************************************************
     csr   $Process      begsr                                                   >---+
     csr                 eval      xxCOUNT=xxCOUNT+1                                 :
     csr                 eval      xxAMTB=xxAMTB+ddAMTB                              :
     csr                 eval      xxAMTR=xxAMTR+ddAMTR                              :
     csr                 eval      xxAMTP=xxAMTP+ddAMTP                              :
     csr                 if        xxCOUNT=1                                     >-+ :
     csr                 eval      xxCO#=ddCO#                                     : :
     csr                 eval      xxFND=ddFND                                     : :
     csr                 eval      xxFYR=ddFYR                                     : :
     csr                 endif                                                   <-+ :
     csr                 if        xxCO#<>ddCO# or xxFND<>ddFND or xxFYR<>ddFYR  >-+ :
     csr                 exsr      $Write_Hd                                       : :
     csr                 exsr      $Write_Bk1                                      : :
     csr                 eval      xxCO#=ddCO#                                     : :
     csr                 eval      xxFND=ddFND                                     : :
     csr                 eval      xxFYR=ddFYR                                     : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxCOUNT1=xxCOUNT1+1                               :
     csr                 eval      xxAMTB1=xxAMTB1+ddAMTB                            :
     csr                 eval      xxAMTR1=xxAMTR1+ddAMTR                            :
     csr                 eval      xxAMTP1=xxAMTP1+ddAMTP                            :
     csr                 eval      xxCOUNT2=xxCOUNT2+1                               :
     csr                 eval      xxAMTB2=xxAMTB2+ddAMTB                            :
     csr                 eval      xxAMTR2=xxAMTR2+ddAMTR                            :
     csr                 eval      xxAMTP2=xxAMTP2+ddAMTP                            :
     csr                 endsr                                                   <---+

      ** Write break 1 line
      *************************************************
     csr   $Write_Bk1    begsr                                                   >---+
     csr                 if        xxCO#<>ddCO# or xxFND<>ddFND                  >-+ :
     csr                 eval      *in61=*on                                       : :
     csr                 eval      xxPRINT=*zero                                   : :
     csr                 endif                                                   <-+ :
     csr                 write     PRTBK1                                            :
     csr                 eval      xxPRINT=xxPRINT+1                                 :
     csr                 eval      xxAMTB1=*zero                                     :
     csr                 eval      xxAMTR1=*zero                                     :
     csr                 eval      xxAMTP1=*zero                                     :
     csr                 eval      xxCOUNT1=*zero                                    :
     csr                 if        *in61=*on                                     >-+ :
     csr                 write     PRTBK2                                          : :
     csr                 eval      *in61=*off                                      : :
     csr                 eval      xxAMTB2=*zero                                   : :
     csr                 eval      xxAMTR2=*zero                                   : :
     csr                 eval      xxAMTP2=*zero                                   : :
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
