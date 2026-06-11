     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*
     H***********************************************************************
     H* Fix Blank Remark Codes
     H***********************************************************************
     H* Create onb 08/07/2014 by c:62 for Support 86382
     H***********************************************************************
     H* Revisions:
     H*
     H***********************************************************************
     FSXDTP     if   e             Disk    Rename(pdt01:pdt0p)
     FSDCMLE    uf   e           k Disk    Rename(pcm01:pcm0e)
     FSMARL1    uf   e           k Disk
     F*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

      /free
                         read pdt0p;
                         DoW Not %Eof(sxdtp);
                            If DTtoRc = ' ';
                               // Update Additional Remarks
                               Chain (DTtoKy:DTtoS#) par01;
                               If %Found(smarl1);
                                  DoW Not %Eof(smarl1);
                                     ARrmCd = DTfrRc;
                                     ARpgm = Q1pgm;
                                     Update par01;
                                     ReadE (DTtoKy:DTtoS#) par01;
                                  EndDo;
                               EndIf;
                               // Update Main Remark
                               Chain (DTtoKy:DTtoS#) pcm0e;
                               If %Found(sdcmle);
                                  CMrmCd = DTfrRc;
                                  CMpgm = Q1pgm;
                                  Update pcm0e;
                               EndIf;
                            EndIf;
                            read pdt0p;
                         EndDo;
      /end-free
     C                   seton                                        lr
