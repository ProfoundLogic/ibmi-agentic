     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* UPDATE FLORIDA EOB X-REF
     H**
     H*****************************************************************
     H* CREATED: 06/03/04 BY C:62 FOR FL046
     H*****************************************************************
     FFTEBPFL   UF   E           K DISK

     D*
     D EOB             S              3    DIM(136) CTDATA PERRCD(1)
     D XREF            S              3    DIM(136) ALT(EOB)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*** GET COMPANY #
     C                   READ      PEB01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     1             X                 3 0
     C                   MOVEL(P)  EBKEY         XXEOB             3
     C     XXEOB         LOOKUP    EOB(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  XREF(X)       EBXREF
     C                   ENDIF
     C*
     C                   UPDATE    PEB01
     C                   READ      PEB01                                  99
     C                   ENDDO
     C*
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   RETURN
** EOB XREF
S01S01
S02S02
S03S03
S04S04
S05S05
S06S06
S07S07
S08S08
S09S09
S10S10
S11S11
S12S12
S13S13
S14S14
S15S15
S16S16
S17S17
S18S18
S19S19
S20S20
001S04
002S08
003S16
004S20
005S17
006S13
007S02
008S20
009S20
010S20
011S20
012S06
013S17
014S05
015S06
016S15
051S04
052S02
053S20
054S08
055S08
056S16
057S20
058S20
059S04
060S20
061S08
062S20
063S01
064S08
065S17
066S05
067S20
068S14
069S20
101S04
102S08
103S13
104S13
105S02
106S20
107S20
108S20
109S20
110S05
111S15
112S20
201S17
202S20
203S13
204S18
205S20
206S20
207S20
208S20
209S20
210S16
211S18
212S20
213S20
214S02
215S17
216S20
217S20
218S20
219S20
220S20
221S20
222S13
223S20
224S20
225S20
226S20
227S20
228S16
229S20
230S15
231S15
232S20
233S20
234S15
235S20
236S20
237S17
238S20
239S04
240S20
241S20
242S20
243S20
244S20
245S20
246S20
247S20
248S20
249S20
250S20
251S20
252S20
253S10
270S20
271S02
272S20
273S20
274S20
275S20
276S17
277S20
278S20
279S20
299S20
300S20
396S09
397S09
398S09
399S20
