     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE FIXBEAST                                                                       *
     H**************************************************************************************
     H*  Description:  Add Beast record for client LUBA.                                   *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 05/30/14   Programmer C:296   Project:MC378                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFMBEL5    UF   E           K DISK
     C*
     C*----------------------------------------------------------------------
     C*  Read through FMBEL5
     C*----------------------------------------------------------------------
     C* Retriving client number.
     C*
     C                   call      'RTVCLTID'
     C                   parm                    ##clt#            3
     C
      /Free
         If ##clt# = '133' ;
           Chain ('P':'X':'LA') PBE01;
           If %found(FMBEL5);

             BEDEPG = 'F0373RLA' ;
             UPDATE PBE01 ;

           Endif ;
         Endif ;
         *inlr = *on ;
      /End-Free
