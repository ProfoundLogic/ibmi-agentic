/* @MBRTEXT: INVENTORY BALANCE INQUIRY FOR ALL STORES */
/* @MBRTYPE: CLP */
/* @#$* Copyright Statement ********* Do not remove *********/
/************************************************************/
/************************************************************/
/******                                                 *****/
/******  Copyright JDA Software Services Inc. All       *****/
/******  rights reserved. This program is protected     *****/
/******  by the copyright Act of 1976, as amended, as   *****/
/******  an unpublished work and the foregoing notice   *****/
/******  and legend shall not be deemed to constitute   *****/
/******  publication or an intent to publish thereunder.*****/
/******  This program is proprietary and confidential   *****/
/******  information of JDA Software Services Inc., and *****/
/******  may be disclosed and used only as authorized   *****/
/******  in the license Agreement controlling such      *****/
/******  disclosure and use.                            *****/
/******                                                 *****/
/************************************************************/
/************************************************************/
             PGM
             DCL        VAR(&INUMBR) TYPE(*DEC) LEN(9 0) VALUE(0)
             CALL       PGM(INV046) PARM(&INUMBR)
             ENDPGM