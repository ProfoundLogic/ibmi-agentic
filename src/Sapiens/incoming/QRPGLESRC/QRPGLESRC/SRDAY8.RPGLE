     H*copyright('Progressive Data Solutions, Inc., 1996')
     H*
     H****************************************************************
     H*  Revisions:
     H*  04/09/98  C:69  Changed program to no longer call SRCVTDAT
     H****************************************************************
     D@@date1          s              8  0
     D@@shortday       s              4
     D@@longday        s              9
     D@@err            s              1
     D*
     Ddate1            s               d   datfmt(*iso)
     Ddate2            s               d   datfmt(*iso) inz(d'1900-01-01')
     Ddiff             s              7  0
     Danswer           s              7  0
     Dremain           s              1  0
     D*
     Dday1             s              4    dim(7) perrcd(1) ctdata
     Dday2             s              9    dim(7) alt(day1)
     C     *entry        plist
     C                   parm                    @@date1
     C                   parm                    @@shortday
     C                   parm                    @@longday
     C                   parm      *in99         @@err
     C*
     C                   move      *blanks       @@shortday
     C                   move      *blanks       @@longday
     C*
     C                   if        @@date1 < 1000000
     C     *mdy          test(d)                 @@date1                99
     C  N99*mdy          move      @@date1       date1
     C                   else
     C     *usa          test(d)                 @@date1                99
     C  N99*usa          move      @@date1       date1
     C                   endif
     C*
     C                   if        *in99 = *off
     C     date1         subdur    date2         diff:*days             99
     C     diff          div       7             answer
     C                   mvr                     remain
     C                   add       1             remain
     C                   if        remain <= 0
     C                   seton                                        99
     C                   else
     C                   movea     day1(remain)  @@shortday
     C                   movea     day2(remain)  @@longday
     C                   endif
     C                   endif
     C*
     C                   return
** CTDATA DAY1
MON.MONDAY
TUE.TUESDAY
WED.WEDNESDAY
THU.THURSDAY
FRI.FRIDAY
SAT.SATURDAY
SUN.SUNDAY
