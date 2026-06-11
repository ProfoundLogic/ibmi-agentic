     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H/TITLE FIXCMSPDS# - Fix DCN# with CMPDS#
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     //==========================================================
      // Notes:
     //==========================================================
     // Created: 09/25/19    C:053   Ticket: 83091
     //==========================================================
      // Revisions:
      //
     //==========================================================
     Fmed2ngcd  uf   e             disk
     Fwmcml1    if   e           k disk
      *
     D xxco#           s                   like(cmco#)
     D*xxfnd           s                   like(cmfnd)
     D*xxfyr           s                   like(cmfyr)
     D*xxcase          s                   like(cmcase)
      *
     D                 ds
     D xxdcn#                        15
     D  xxfnd                         3  0 overlay(XXdcn#)
     D  xxfyr                         3  0 overlay(XXdcn#:4)
     D  xxcase                        7  0 overlay(XXdcn#:7)
      *
     C     cmky          KLIST
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxcase
      /Free
         xxco# = 777;
         Read  pcd;
         Dow Not %Eof(med2ngcd);
            xxdcn# = cddcn#;
            Chain cmky pcm01;
               If Not %Eof(wmcml1);
                  If cmpds# <> *Blanks;
                     cddcn# = cmpds#;
                     Update pcd;
                  Endif;
               Endif;
         Read  pcd;
         Enddo;

         *Inlr = *On;

      /End-free
