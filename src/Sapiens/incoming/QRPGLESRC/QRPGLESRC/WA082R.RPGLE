     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE WA082R - RETURN OCR LINE
     H*****************************************************************
     H** PROGRAMMER: C:235                                            *
     H** CREATED:    04/07/11         PROJECT: 114WYM                 *
     H**                                                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
    H*                                                               *
     H*****************************************************************
     D oc2             S              1    DIM(39)                              SCANLINE
     D wt2             S              1    DIM(39) CTDATA PERRCD(39)            WEIGHT
     D                 DS
     D  XXscan                 1     41
     D  dsfnd                  1      3
     D  dsemp#                 4     12
     D  dsfyr                 13     15
     D  dsdiv                 16     20
     D  dsdudt                21     28
     D  dstotl                29     39
     D  dsckdg                40     40  0
     D  dsnegt                41     41
     D  @@totl         s             10  2
     D  xx2n           s              2  0
     D  xxttl          s              2  0
      *
     C******
     C     *like         define    qqamt         xxtotl
     C******
      *
      *  ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   parm                    qqco#             3 0
     C                   parm                    qqfnd             3 0
     C                   parm                    qqemp#            9 0
     C                   parm                    qqfyr             3 0
     C                   parm                    qqdiv             5 0
     C                   parm                    qqdate            8 0
     C                   parm                    qqamt            11 2
     C                   parm                    qqscan           41
     C******
     C                   clear                   XXSCAN
     C                   move      qqfnd         dsfnd
     C                   move      qqemp#        dsemp#
     C                   move      qqdiv         dsdiv
     C                   move      qqfyr         dsfyr
     C                   move      qqdate        dsdudt
     C                   moveL     '0'           dstotl
     C                   move      qqamt         xxtotl
     C                   move      xxtotl        dstotl
     C                   move      ' '           dsnegt
     C******
     C                   if        xxtotl < *zero
     C                   eval      @@totl = xxtotl * -1
     C                   move      @@totl        dstotl
     C                   move      '-'           dsnegt
     C                   endif
     C*** CHECK DIGIT CALC ********************************************
     C                   movel     xxscan        xxoc39           39
     C                   movea     xxoc39        oc2
     C                   z-add     *zeros        xxsum
     C     1             DO        39            A                 2 0
     C                   move      oc2(A)        xxln              1 0
     C                   move      wt2(A)        xxrn              1 0
     C                   eval      xx2n = xxln * xxrn
     C                   movel     xx2n          xxln
     C                   move      xx2n          xxrn
     C                   eval      xxttl = xxln + xxrn
     C                   add       xxttl         xxsum             5 0
     C                   enddo
     C     xxsum         div       10            xxwrk             2 0
     C                   mvr                     xxrem             1 0
     C     10            sub       xxrem         dsckdg
     C                   move      xxscan        qqscan
     C                   SETON                                            LR
     C****************************************************************
** wt2 {Weight}
753275327532753275327532753275327532753
