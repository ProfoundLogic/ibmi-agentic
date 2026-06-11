     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IA001R - Reassign Queue
     H*****************************************************************
     H* Created: 08/06/2014 C:296 Project: 114MNV
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program reasign queue for the document and remove     *
     H*    it from current Queue.                                     *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*  CALLS TO: *None                                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*****************************************************************
     F*
     FImiql3    if   e           k disk
     FImielh    if a e           k disk
     FImdll     if   e           k disk
     FImdll4    uf   e           k disk
     F                                     Rename(pdl01:pdl02)
     F*** LOGICAL OF IMDOP
     FImdol1    if   e           k disk
     FSmsll2    if   e           k disk
     FImielc    uf   e           k disk
     F                                     Rename(pie01:pie02)
     F*
     F***********************************************************
     D*
     DDocRcvDt         s              8  0
     D*
     D @@DcId          s             12
     D @@OutQ          s             10
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*
     C* Entry Parameter
     C     *entry        Plist
     C                   Parm                    @@DcId
     C                   Parm                    @@OutQ

      /Free
       //====================================================================*
       //Program Starts Here
       //====================================================================*

       ExSr MainSr;
       *inlr = *on;

       //====================================================================*
       //  Main Subroutine
       //====================================================================*
         BegSr MainSr;

         ExSr ReAssignQue;

         EndSr;

       //====================================================================*
       //  Subroutine to reassign Queue.
       //====================================================================*
         BegSr ReAssignQue;

           //----------------------------------------------------------------*
           //Get document receive date.
           //----------------------------------------------------------------*
           Chain (@@DcId) pdo01;
             If %found(Imdol1);
               DocRcvDt = DOrcvd;

           //----------------------------------------------------------------*
           //When doc not present in Imdop
           //check Imiep for receive date.
           //----------------------------------------------------------------*
             Else;
               Chain (@@DcId) pie02;
                 If %found(Imielc);
                   DocRcvDt = IErcvD;
                 Endif;
             Endif;

           //--------------------------------------------------------------*
           //Reasign Queue after marking 'D' for existing queue.
           //--------------------------------------------------------------*
           Setll (%trim(@@OutQ):%trim(@@DcId)) pie01;
             If %equal(Imielh);
                LeaveSr;             // doc is already in new outq
             Else;
             //--------------------------------------------------------------*
             //Remove entry for existing record, by making its status 'D'
             //when it is not the new out-queue to be transfered.
             //--------------------------------------------------------------*
                Chain (@@DcId) pie02;
                If %found(Imielc);
                   IEstat = 'D';
                   IElDt  = %dec(%date():*ymd);
                   IElUsr = q1User;
                   IElTim = %Dec(%Time():*hms);
                   IElPgm = q1Pgm;

                   Update pie02;
                   //------------------------------------------------------------*
                   //After changing the status of existing record to D,
                   //write the new entry which contains new Queue.
                   //------------------------------------------------------------*
                   IEqId = @@OutQ;
                   IEdcId = @@DcId;
                   IEstat = *Blanks;
                   IEiDt = %dec(%date():*ymd);
                   IElDt = %dec(%date():*ymd);
                   IElUsr = q1User;
                   IEiUsr = q1User;
                   IElTim = %Dec(%Time():*hms);
                   IEiTim = %Dec(%Time():*hms);
                   IElPgm = q1Pgm;
                   IEiPgm = q1Pgm;
                   IErcvD = DocRcvDt;

                   Write Pie01;
                Endif;

             Endif;

         EndSr;

      /End-Free
