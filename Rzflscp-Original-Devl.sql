DROP FUNCTION UNM.RZFLSCP;

CREATE OR REPLACE FUNCTION UNM.RZFLSCP( BANNER_ONE_UP_NUM IN NUMBER DEFAULT NULL )
   RETURN NUMBER
IS
   --===========================================================================
   --   filename: RZFLSCP.sql           Function to Load SCholarship Parameters
   --            ==> LSCP  derived from the leters: ~    ~~          ~
   --===========================================================================

   --===========================================================================
   -- The CURRENT_RELEASE (Version) Number is the year created with a decimal
   -- part equal to the date of the latest change.
   -- E.g., created in 2003 and current release December 13, 2010 is:
   --                       2003.20101213 [YYYY MM DD].
   --                                                          --das, 04/16/2012
   -----------------------------------------------------------------------------
   CURRENT_RELEASE               CONSTANT NUMBER := 2012.20130102;

   G_NUMBER                      NUMBER;
   FUNCTION_STATUS               NUMBER;

   C_RPT_DATE_STR                CONSTANT VARCHAR2(20 CHAR)  DEFAULT TO_CHAR(SYSDATE,'YYYYMMDDHH24MI');
   ONE_UP_NUMBER                 NUMBER := NULL;
   G_INPUT_HANDLE                Utl_File.FILE_TYPE;
   G_HISTORY_HANDLE              Utl_File.FILE_TYPE;
   G_BASE_FILENAME               VARCHAR2(  64 CHAR) DEFAULT NULL;
   G_INPUT_FILENAME              VARCHAR2(  64 CHAR) DEFAULT NULL;
   G_HISTORY_FILENAME            VARCHAR2(  64 CHAR) DEFAULT NULL;
   G_FILE_DIRECTORY              VARCHAR2(  64 CHAR) := 'FINAID_SCHOLARSHIP_LOAD';
   G_FILE_HISTORY_DIRECTORY      VARCHAR2(  64 CHAR) := 'FINAID_SCH_LOAD_HISTORY';
   G_FILE_EXTENTION              VARCHAR2(  20 CHAR) := '.txt';
   HT                            VARCHAR2(  10 CHAR) := CHR(9);
   CRLF                          VARCHAR2(   5 CHAR) := Chr(13) || Chr(10);
   PCNT                          VARCHAR2(   5 CHAR) := Chr(37);
   USER_NAME                     VARCHAR2(  50 CHAR);
   MAILTO_ADDRESS                VARCHAR2( 100 CHAR);
   INSTANCE_NAME                 VARCHAR2(  50 CHAR);
   -- String length constraints must be in range (1 .. 32767)
   G_MESSAGE_BODY                VARCHAR2(9000 CHAR);    --7500 CHAR);
   TERMS_NOT_OK                  BOOLEAN;

   NMX                           NUMBER := 25;
   CNT                           NUMBER;
   XX                            NUMBER;
   DIGIT_COUNT                   NUMBER;
   CURRENT_AIDY                  VARCHAR2( 10 CHAR);
   CURRENT_TERM                  VARCHAR2( 10 CHAR);

   DATA_IMPORT_FILE_EXISTS       BOOLEAN := FALSE;
   EXISTS_FLAG                   BOOLEAN;
   FILE_LENGTH                   NUMBER;
   BLOCKSIZE                     NUMBER;

   INTERNAL_VERSION              NUMBER;

   G_UNM_BANID                   VARCHAR2( 100 CHAR);
   G_AWARD_AIDY                  VARCHAR2( 100 CHAR);
   G_FUND_CODE                   VARCHAR2( 100 CHAR);
   G_AWARD_ACTION                VARCHAR2( 100 CHAR) := 'ACCP';
   G_AWARD_TERM                  VARCHAR2( 100 CHAR);
   G_AWARD_AMOUNT                NUMBER;
   G_RRRAREQ_TREQ_CODE           VARCHAR2(  10 CHAR);
   G_RRRAREQ_TRST_CODE           VARCHAR2(   4 CHAR);
   G_RRRAREQ_PERIOD              VARCHAR2(  15 CHAR);
   RRRAREQ_LOAD_OK               NUMBER := 0;
   RRRAREQ_LOAD_NOGO             NUMBER := 0;
   RRRAREQ_LOAD_NULLS            NUMBER := 0;
   ANALYSIS_NOTE                 VARCHAR2( 100 CHAR);
   ANALYSIS_CODE                 NUMBER;


   SOME_DATA                     BOOLEAN;
   SOME_COMMENT_ONLY_DATA        NUMBER := 0;
   LOAD_CHECK                    BOOLEAN;
   RHRCOMM_LOAD_OK               NUMBER := 0;
   RHRCOMM_LOAD_FAILED           NUMBER := 0;
   RORMESG_LOAD_OK               NUMBER := 0;
   RORMESG_LOAD_FAILED           NUMBER := 0;

   G_TEXT_FOR_RHRCOMM            VARCHAR2(4000 CHAR);
   G_RHRCOMM_CAT_CODE            VARCHAR2(  10 CHAR);
   G_EXDY_FOR_RORMESG            NUMBER;
   G_CODE_FOR_RORMESG            VARCHAR2(  10 CHAR);
   G_TEXT_FOR_RORMESG            VARCHAR2(2000 CHAR);
   G_BANNER_PIDM                 NUMBER;
   G_RORMESG_DATE_PERIOD         NUMBER;
   G_ACADEMIC_LOAD               VARCHAR2(   5 CHAR);
   G_AWRD_LTR_IND                VARCHAR2(   5 CHAR);
   G_NEXT_FASP_TERM              VARCHAR2(  10 CHAR);

   PCT_TERM                      VARCHAR2(  10 CHAR);
   PCT_AIDY                      VARCHAR2(  10 CHAR);
   AIDY_AWARD_COUNT              NUMBER := 0;
   TERM_AWARD_COUNT              NUMBER := 0;

   RZRSCIN_IND                   NUMBER := 0;
   UNATHORIZED_USER              NUMBER := 0;
   TOTAL_FIELD_ERRORS            NUMBER := 0;
   FILE_READ_ERRORS              NUMBER := 0;
   FILE_READ_COUNT               NUMBER := 0;
   FILE_DATA_LENGTH              NUMBER := 0;
   TABLE_INSERT_COUNT            NUMBER := 0;
   UNUSABLE_DATA_COUNT           NUMBER := 0;
   DOWNLOAD_DATA                 VARCHAR2(25000 CHAR);
   AWARDING_DATA                 VARCHAR2(  500 CHAR);
   --=======================================================
   ERR_NOTE_CHARS_MAX            CONSTANT NUMBER := 50;
   --=======================================================
   --=======================================================
   BAD_CHARS                     VARCHAR2(500 CHAR) := '';
   BAD_CHARS2                    VARCHAR2(500 CHAR) := '';
   EXISTS_FLAG2                  BOOLEAN;
   F                             NUMBER;
   N                             NUMBER;
   INDX                          NUMBER;

   L_TERM                        VARCHAR2(  6 CHAR);
   L_AIDY                        VARCHAR2(  6 CHAR);
   L_FUND                        VARCHAR2(  6 CHAR);

--   TYPE tString IS TABLE OF VARCHAR2(5000) INDEX BY BINARY_INTEGER;
   L_FLD                         UNM.ARRAYTABLES.TSTRING;
   L_FLD_DESC                    UNM.ARRAYTABLES.TSTRING;
   L_FLD_ERRDATA                 UNM.ARRAYTABLES.TSTRING;
   L_FLD_ERR                     UNM.ARRAYTABLES.TNUMBER;
   L_FLD_UNQERR                  UNM.ARRAYTABLES.TNUMBER;
   L_FLD_MSG_ORDER               UNM.ARRAYTABLES.TNUMBER;
   DATA_IS_OK                    BOOLEAN := FALSE;
   MAX_FIELDS                    NUMBER  := 19;

   L_ERROR_FIELD                 NUMBER  := 0;

   FIELD_COUNT                   NUMBER;
   CHAR_TRKR                     UNM.ARRAYTABLES.TNUMBER;
   CHAR_TRKR_CNT                 NUMBER := 0;
   CHAR_TRKR_MAX_CNT             NUMBER := 100;
   CHAR_TRKR_ROW_CNT             NUMBER := 0;
   ROW_COUNT                     NUMBER;
   SPECIAL_FUND                  UNM.ARRAYTABLES.TSTRING;
   SP_FUND_COUNT                 NUMBER := 0;
   FUND_ECPTN_CNT                NUMBER := 0;
   TOTAL_PARAM_DATA_ROWS         NUMBER := 0;
   PARAM_DATA_ROWS_WITH_AWARD    NUMBER := 0;
   PCKG_LOAD_IND                 NUMBER := 0;
   PCKG_LOAD_OK                  NUMBER := 0;
   PCKG_LOAD_NULLS               NUMBER := 0;
   AWRD_LTR_IND_FLAGS            NUMBER := 0;
   AWRD_LTR_IND_NULLS            NUMBER := 0;
   AWRD_LTR_IND_YES              NUMBER := 0;
   AWRD_LTR_IND_NO               NUMBER := 0;
   TOTAL_COMMENT_ROWS            NUMBER := 0;
   COMMENT_ROWS_WITH_AWARD       NUMBER := 0;
   COMMENT_ROWS_NO_AWARD         NUMBER := 0;
   RHACOMM_CAT_CODES             NUMBER := 0;
   RHACOMM_DATA                  NUMBER := 0;
   RORMESG_DATA                  NUMBER := 0;
   RHACOMM_NO_AWARD              NUMBER := 0;
   RHACOMM_WITH_AWARD            NUMBER := 0;
   RORMESG_NO_AWARD              NUMBER := 0;
   RORMESG_WITH_AWARD            NUMBER := 0;
   TMPNUMBER                     NUMBER;
   TMPSTRING                     VARCHAR2( 500 CHAR);
   TMPSTRING2                    VARCHAR2( 500 CHAR);
   TMPSTRING3                    VARCHAR2( 500 CHAR);
   OK_TO_PROCESS_INPUT_DATA      BOOLEAN := FALSE;
   OK_TO_CALL_RZKSCLD            BOOLEAN := FALSE;
   OK_TO_WRITE_NOTES_ONLY        BOOLEAN := FALSE;

/*===============================================================================
== UNM.RZRSCIN ~~~ Scholarship Tool Input Parameter Data
==    RZRSCIN_AIDY_CODE     ~~~ AID Year Code: The aid year associated with the information in this record.
==    RZRSCIN_UNM_ID        ~~~ UNM_ID: Internal system-generated student identification number.
==    RZRSCIN_FUND_CODE     ~~~ Fund Code: The Financial Aid code for the award processing.
==    RZRSCIN_TERM_CODE     ~~~ [optional] Term (Period) Code: The school period associated with the information in this record.
==    RZRSCIN_AWARD_AMT     ~~~ Award Amount: The Financial Aid monetary amount for the award processing.
==    RZRSCIN_AWST_CODE     ~~~ Award Action: The code (ACCP or CANC) for the award processing.
==    RZRSCIN_AWRD_LTR_IND  ~~~ [optional] AWARD LETTER INDICATOR: Indicates whether an award letter should be generated for the applicant.
==    RZRSCIN_PCKG_LOAD_IND ~~~ [optional] PACKAGING LOAD INDICATOR: The enrollment status for this period.
==    RZRSCIN_RRRAREQ_TREQ  ~~~ [optional] RRRAREQ TREQ Code: The code associated with the tracking requirement.';
==    RZRSCIN_RRRAREQ_TRST  ~~~ [optional] RRRAREQ TRST Code: The status of the tracking requirement.';
==    RZRSCIN_RRRAREQ_PRD   ~~~ [optional] RRRAREQ PERIOD: The period associated with the requirement; valid only if RTVTREQ_TERM_ELIGIBLE_IND = "Y"';
==    RZRSCIN_RHRCOMM_Cat   ~~~ [optional] RHRCOMM Category: The category associated with the RHRCOMM comment.
==    RZRSCIN_RHRCOMM_Txt   ~~~ [optional] RHRCOMM Text: The text of the RHRCOMM comment.
==    RZRSCIN_RORMESG_EXPR  ~~~ [optional] EXPIRATION DATE: Date the message expires.
==    RZRSCIN_RORMESG_CODE  ~~~ [optional] RORMESG Code: The code associated with the RORMESG comment.
==    RZRSCIN_RORMESG_Txt   ~~~ [optional] RORMESG Full Text: The text of the RORMESG comment(s).
===============================================================================*/

   /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Returns the API/Package Version number.
   *        Note: An application programming interface (API) is an interface that a
   *              software program implements in order to allow other software to
   *              interact with it.
   * Version number of this package. Changes only when the signature changes.
   *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
   FUNCTION F_API_VERSION
         RETURN NUMBER IS
      BEGIN
         RETURN (CURRENT_RELEASE);
   END F_API_VERSION;

   FUNCTION UNALLOWED_CHARACTER_INPUT( STRING IN VARCHAR2 )
         RETURN BOOLEAN
   IS
      J                       NUMBER;
      K                       NUMBER;
      STRING_LENGTH           NUMBER;
      STRING_2                VARCHAR2(500 CHAR);
      STRING_3                VARCHAR2(500 CHAR);
      NOT_FOUND               BOOLEAN := TRUE;
      CHAR_ERR                BOOLEAN := FALSE;
   BEGIN
      K := CHAR_TRKR_CNT;
      STRING_LENGTH := Nvl(Length( STRING ), 0);
      IF (STRING_LENGTH > 0) THEN
         FOR I IN 1..STRING_LENGTH LOOP
            J := Ascii(SubStr(STRING, I, 1));
            IF ((J != 46) AND (J NOT BETWEEN 0 AND 31) AND (J NOT BETWEEN 48 AND 57) AND (J NOT BETWEEN 65 AND 90)) THEN
               CHAR_ERR := TRUE;
               IF (CHAR_TRKR_CNT = 0) THEN
                  CHAR_TRKR_CNT := CHAR_TRKR_CNT + 1;
                  CHAR_TRKR(CHAR_TRKR_CNT) := J;
               ELSE
                  FOR C IN 1..CHAR_TRKR_CNT LOOP
                     IF (CHAR_TRKR(C) = J) THEN
                        NOT_FOUND := FALSE;
                     END IF;
                  END LOOP;
                  IF NOT_FOUND THEN
                     CHAR_TRKR_CNT := CHAR_TRKR_CNT + 1;
                     CHAR_TRKR(CHAR_TRKR_CNT) := J;
                  END IF;
               END IF;
            END IF;
         END LOOP;
      END IF;
      IF CHAR_ERR THEN
         CHAR_TRKR_ROW_CNT := CHAR_TRKR_ROW_CNT + 1;
-- -- -- Dbms_Output.Put_Line( '~~~~~~~~~~ Unallowed_Character_Input [char_trkr_row_cnt: '||char_trkr_row_cnt||', char_trkr_cnt: '||char_trkr_cnt||'] String ==>'||String||'<==  has problem(s)' );
         RETURN TRUE;
      ELSE
         RETURN FALSE;
      END IF;
   END UNALLOWED_CHARACTER_INPUT;

   FUNCTION APPEND_TEXT(STRING IN OUT VARCHAR2, TEXT IN VARCHAR2,
                        DLMTR  IN VARCHAR2,     MAX_CHAR_FOR_STRING IN NUMBER)
         RETURN BOOLEAN
   IS
      TEXT_LENGTH             NUMBER;
      STRING_LENGTH           NUMBER;
      DELIMITER_LENGTH        NUMBER;
      NEW_STRING_LENGTH       NUMBER;
      DELIMITER               VARCHAR2(5 CHAR);
      UNIQUE_TEXT             BOOLEAN := TRUE;

   BEGIN
      STRING_LENGTH := Length( Nvl(STRING, ' ' ));
      IF ( Nvl(STRING, ' ' ) = ' ' ) THEN
         STRING := TEXT;
      ELSE
         DELIMITER         := SubStr( Nvl( DLMTR, '--' ), 1, 5 );
         DELIMITER_LENGTH  := Length( DELIMITER );
         TEXT_LENGTH       := Length( TEXT );
         NEW_STRING_LENGTH := STRING_LENGTH + DELIMITER_LENGTH + TEXT_LENGTH;
         IF ( InStr( STRING, TEXT ) = 0 ) THEN
            IF ( NEW_STRING_LENGTH < MAX_CHAR_FOR_STRING ) THEN
               STRING := STRING || DELIMITER || TEXT;
            END IF;
         ELSE
            UNIQUE_TEXT := FALSE;
         END IF;
      END IF;

      RETURN UNIQUE_TEXT;
   END APPEND_TEXT;

   PROCEDURE APPEND_ERROR_NOTE( STRING IN OUT VARCHAR2, TEXT IN VARCHAR2, NUMB IN OUT NUMBER )
   IS
      MAX_LEN           CONSTANT NUMBER := 50;
   BEGIN
      IF (APPEND_TEXT(STRING, TEXT, '~', MAX_LEN)) THEN
         NUMB := NUMB + 1;
      END IF;
   END APPEND_ERROR_NOTE;


      /*========================================================================================
         RZBSCAM -- Financial Aid Scholarship Fund Award Amounts; including the Default Award
                        Amount for the fund in the aid year.
         RZBSCFR -- The Financial Aid Fund Rules - Used to determine which funds are mutually
                        exclusive and which funds need adjustment with other funds.
         RZBSCLU -- The Scholarship Award/Denial List lookup table. Includes:
                        The limit of PE courses per term.
                        The maximum number of terms a person is allowed have for this fund code.
                        The column number of the 0506 ROBUSDF table with fund terms used.
                        The first term, year, & continuing gpa and hours required.
         RFRASPC -- Fund Aid Year Specific Data; including the available amount to offer:
                        This field represents the dollar amount of the fund that can be
                        offered including any over-commitment amount. This field will be
                        calculated if an Available to Offer Percent is entered.
      ------  ------  ------  ------  ------  ------  ------  ------
            SELECT rfraspc_avail_offer_amt
            FROM RFRASPC
            WHERE rfraspc_aidy_code = p_aidy
              AND rfraspc_fund_code = p_fund;
      ========================================================================================*/

   FUNCTION DATA_ISSUE_33(IDX IN NUMBER, RAW_DATA IN VARCHAR2) RETURN BOOLEAN
   IS
   BEGIN
-- -- -- Dbms_Output.Put_Line( '~~~~~~~~~~ DATA_Issue_33(idx ==>'||idx||'<==  RAW_Data: '||Nvl(RAW_Data, 'null')||' )' );
-- Dbms_Output.Put_Line( '~~~~~~~~~~ DATA_Issue_33(idx ==>'||idx||'<==  RAW_Data: '||Nvl(RAW_Data, 'null')||' )' );

      L_FLD_ERR(IDX) := L_FLD_ERR(IDX) + 1;
      APPEND_ERROR_NOTE(L_FLD_ERRDATA(IDX), RAW_DATA, L_FLD_UNQERR(IDX));

-- Dbms_Output.Put_Line( '~~~~~~~~~~ DATA_Issue_33(idx ==>'||idx||'<==  RAW_Data: '||Nvl(RAW_Data, 'null')||' )  ==>'||L_fld_err(idx)||'<==>'||L_fld_unqerr(idx) );
      RETURN FALSE;
   END DATA_ISSUE_33;

   PROCEDURE PROC_AWRD_LTR_CK( P7 IN OUT VARCHAR2, DATA_LOOKS_GOOD IN OUT BOOLEAN )
   IS
      STR1              VARCHAR2( 25 CHAR);
   BEGIN
      STR1 := Upper(Nvl(P7, 'Y'));
      IF ((STR1 != 'Y') AND (STR1 != 'N')) THEN
         DATA_LOOKS_GOOD := DATA_ISSUE_33(7, Nvl(P7, 'Y'));
      ELSE
         P7 := Upper(P7);
         DATA_LOOKS_GOOD := TRUE;
      END IF;
   END PROC_AWRD_LTR_CK;

   PROCEDURE PROC_ACAD_LD_CK( P8 IN VARCHAR2, DATA_LOOKS_GOOD IN OUT BOOLEAN, ACLD OUT VARCHAR2 )
   IS
      LOAD              NUMBER := -1;
      STR1              VARCHAR2( 25 CHAR);
   BEGIN
      STR1 := Nvl(P8, '1');
      IF (UNM.RZKFLIB.IS_NUMERIC(STR1) = TRUE) THEN
         LOAD := To_Number(STR1);
         IF ((LOAD < 1) OR (LOAD > 5)) THEN
            LOAD := -1;
            DATA_LOOKS_GOOD := DATA_ISSUE_33(8, Nvl(P8, 'NULL'));
         ELSE
            DATA_LOOKS_GOOD := TRUE;
         END IF;
      ELSE
         LOAD := -1;
         DATA_LOOKS_GOOD := DATA_ISSUE_33(8, Nvl(P8, 'NULL'));
      END IF;
      ACLD := Trim(To_Char(LOAD));
   END PROC_ACAD_LD_CK;

   PROCEDURE PROC_ACTION_CK( P6 IN VARCHAR2, DATA_LOOKS_GOOD IN OUT BOOLEAN )
   IS
      L_ACTION                   VARCHAR2(  6 CHAR);
   BEGIN
      L_ACTION := SubStr(Nvl(P6, 'NULL'), 1, 4);
      IF ((L_ACTION != 'ACCP') AND (L_ACTION != 'CANC')) THEN
         DATA_LOOKS_GOOD := DATA_ISSUE_33(6, L_ACTION);
      ELSE
         DATA_LOOKS_GOOD := TRUE;
      END IF;
   END PROC_ACTION_CK;

   FUNCTION AWARD_AMOUNT_CHECK(FP_AIDY IN VARCHAR2, FP_FUND IN VARCHAR2, FP_AMOUNT IN NUMBER, FP_STATUS OUT NUMBER)
         RETURN BOOLEAN
      IS
         UPPER_AMOUNT      NUMBER;
         LOWER_AMOUNT      NUMBER;
         ALLOCATED_AMOUNT  NUMBER;

      BEGIN
         FP_STATUS := 10;
         SELECT Max(MAX_AMT), Max(MIN_AMT), Max(ALLOC_AMT)
         INTO UPPER_AMOUNT, LOWER_AMOUNT, ALLOCATED_AMOUNT
         FROM
            (
            SELECT RFRASPC_MAX_AWARD_AMT  MAX_AMT, RFRASPC_MIN_AWARD_AMT  MIN_AMT, RFRASPC_TOTAL_ALLOC_AMT  ALLOC_AMT
            FROM RFRASPC WHERE RFRASPC_AIDY_CODE = FP_AIDY AND RFRASPC_FUND_CODE = FP_FUND
         UNION
            SELECT 0 MAX_AMT, 0 MIN_AMT, 0 ALLOC_AMT FROM DUAL
            );

         CASE
            WHEN FP_AMOUNT > UPPER_AMOUNT     THEN FP_STATUS :=  1;
            WHEN FP_AMOUNT < LOWER_AMOUNT     THEN FP_STATUS := -1;
            WHEN FP_AMOUNT > ALLOCATED_AMOUNT THEN FP_STATUS :=  5;
            ELSE FP_STATUS := 0;
         END CASE;

--          Dbms_Output.Put_Line( 'Award_Amount_Check('||fp_AIDY||', '||fp_FUND||', '||fp_Amount||', Status ==> '||fp_Status||')' );
         IF (FP_STATUS = 0) THEN
            RETURN TRUE;
         ELSE
            RETURN FALSE;
         END IF;
      EXCEPTION WHEN OTHERS THEN
         RETURN FALSE;
   END AWARD_AMOUNT_CHECK;

   PROCEDURE PROC_AMOUNT_CK( P5 IN VARCHAR2, DATA_LOOKS_GOOD IN OUT BOOLEAN )
   IS
      AMOUNT_OK            BOOLEAN := FALSE;
      AMOUNT_STATUS        NUMBER  := 10;
   BEGIN
      IF (UNM.RZKFLIB.IS_NUMERIC(P5) = TRUE) THEN
         AMOUNT_OK := AWARD_AMOUNT_CHECK(L_AIDY, L_FUND, P5, AMOUNT_STATUS );

--          IF (p5 < 0) THEN
--             DATA_Looks_Good := DATA_Issue_33(5, Nvl(p5, 'NULL'));
--                L_fld_desc( 5) := ' AMOUNT non-Numeric.........:';     L_fld_MSG_Order( 5) :=  4;
--                L_fld_desc(19) := ' AMOUNT Exceeds Fund Max-Min:';     L_fld_MSG_Order(19) :=  5;
         IF (AMOUNT_STATUS != 0) THEN
            DATA_LOOKS_GOOD := DATA_ISSUE_33(19, Nvl(P5, 'NULL')||'['||L_FUND||']');
         ELSE
            DATA_LOOKS_GOOD := TRUE;
         END IF;
      ELSE
         DATA_LOOKS_GOOD := DATA_ISSUE_33(5, Nvl(P5, 'NULL'));
      END IF;
   END PROC_AMOUNT_CK;

   PROCEDURE PROC_TERM_CODE_CK( P1 IN VARCHAR2, AIDY_OK IN BOOLEAN, P4 IN VARCHAR2, DATA_LOOKS_GOOD IN OUT BOOLEAN )
   IS
   BEGIN
      L_TERM := Nvl(P4, 'NULL');
      IF L_TERM = P4 THEN
         IF ((L_TERM < CURRENT_TERM) OR
             (RZKFLIB.TERM_NOT_VALID( L_TERM )) OR
             (UNM.RZKFLIB.IS_NUMERIC( P4 ) = FALSE)) THEN
            DATA_LOOKS_GOOD := DATA_ISSUE_33(4, P4);
         ELSE
            L_ERROR_FIELD := 14;
            IF AIDY_OK THEN
               IF (RZKFLIB.AIDY_TERM_MISMATCH( L_AIDY, L_TERM )) THEN
                  DATA_LOOKS_GOOD := DATA_ISSUE_33(14, P1||'~'||P4);
               ELSE
                  DATA_LOOKS_GOOD := TRUE;
               END IF;
            END IF;
         END IF;
      ELSIF (L_TERM = 'NULL') THEN
         DATA_LOOKS_GOOD := TRUE;
      END IF;
   END PROC_TERM_CODE_CK;

   PROCEDURE PROC_FUND_CODE_CK( P3 IN VARCHAR2, DATA_LOOKS_GOOD IN OUT BOOLEAN )
   IS
--       L_FUND                     VARCHAR2(  6 CHAR);
      L_FUND_TMP                 VARCHAR2(  6 CHAR);
   BEGIN
      L_FUND := Nvl(P3, 'NULL');
      L_FUND_TMP := '#####';
      IF (SP_FUND_COUNT > 0) THEN
         FOR S IN 1..SP_FUND_COUNT LOOP
            IF (L_FUND = SPECIAL_FUND(S)) THEN
               L_FUND_TMP := L_FUND;
               EXIT;
            END IF;
         END LOOP;
      END IF;

      IF (L_FUND_TMP = '#####') THEN   -- ...not an exception, so check against rule table
         SELECT Max(FUND) FUND_CODE INTO L_FUND
         FROM
               (
               SELECT RZBSCAM_FUND_CODE FUND FROM RZBSCAM WHERE RZBSCAM_FUND_CODE = L_FUND
            UNION
               SELECT '#####' FUND FROM DUAL
               );
      END IF;

      IF L_FUND != P3 THEN
         DATA_LOOKS_GOOD := DATA_ISSUE_33(3, Nvl(P3, 'NULL'));
      ELSE
         DATA_LOOKS_GOOD := TRUE;
      END IF;
   END PROC_FUND_CODE_CK;

   PROCEDURE PROC_RORMESG_DATE_CK( P14 IN VARCHAR2, DATA_LOOKS_GOOD IN OUT BOOLEAN, PERIOD OUT NUMBER )
   IS
      L_SPECIAL_NOTE             VARCHAR2( 25 CHAR);
      NUMBER_OF_DAYS             NUMBER := 30;
      L_MX_STR_LEN               NUMBER := 25;
   BEGIN
      L_SPECIAL_NOTE := SubStr(Upper(Nvl(P14, 'N/A')), 1, L_MX_STR_LEN);
      IF (L_SPECIAL_NOTE != 'N/A') THEN
         IF UNM.RZKFLIB.IS_NUMERIC(L_SPECIAL_NOTE) THEN
            NUMBER_OF_DAYS := To_Number(L_SPECIAL_NOTE);
            IF ((NUMBER_OF_DAYS < 1) OR (NUMBER_OF_DAYS > 365)) THEN
               NUMBER_OF_DAYS  := -1;
               DATA_LOOKS_GOOD := DATA_ISSUE_33(14, L_SPECIAL_NOTE);
            ELSE
               DATA_LOOKS_GOOD := TRUE;
            END IF;
         ELSE
            NUMBER_OF_DAYS  := -1;
            DATA_LOOKS_GOOD := DATA_ISSUE_33(14, L_SPECIAL_NOTE);
         END IF;
      END IF;
      PERIOD := NUMBER_OF_DAYS;
   END PROC_RORMESG_DATE_CK;

   PROCEDURE PROC_RORMESG_CAT_CK( P15 IN VARCHAR2, DATA_LOOKS_GOOD IN OUT BOOLEAN )
   IS
      L_SPECIAL_NOTE             VARCHAR2( 25 CHAR);
      L_STRING                   VARCHAR2( 25 CHAR);
      L_MX_STR_LEN               NUMBER  := 25;
   BEGIN
      L_SPECIAL_NOTE := SubStr(Upper(Nvl(P15, 'N/A')), 1, L_MX_STR_LEN);
      IF (L_SPECIAL_NOTE != 'N/A') THEN
         SELECT Max(CAT_CODE) INTO L_STRING
         FROM
               (
               SELECT RTVMESG_CODE CAT_CODE FROM RTVMESG WHERE RTVMESG_CODE = L_SPECIAL_NOTE
            UNION
               SELECT '#####' CAT_CODE FROM DUAL
               );

         IF (L_STRING != P15) THEN
            DATA_LOOKS_GOOD := DATA_ISSUE_33(15, Trim(SubStr(Nvl(P15, 'NULL'), 1, 5)));
         ELSE
            DATA_LOOKS_GOOD := TRUE;
         END IF;
      END IF;
   END PROC_RORMESG_CAT_CK;

   PROCEDURE PROC_RHRCOMM_CAT_CK( P12 IN VARCHAR2, DATA_LOOKS_GOOD IN OUT BOOLEAN )
   IS
      L_SPECIAL_NOTE             VARCHAR2( 25 CHAR);
      L_STRING                   VARCHAR2( 25 CHAR);
      L_MX_STR_LEN               NUMBER  := 25;
   BEGIN
      L_SPECIAL_NOTE := SubStr(Upper(Nvl(P12, 'N/A')), 1, L_MX_STR_LEN);
      IF (L_SPECIAL_NOTE != 'N/A') THEN
         SELECT Max(CAT_CODE) INTO L_STRING
         FROM
               (
               SELECT RTVCCOM_CODE CAT_CODE FROM RTVCCOM WHERE RTVCCOM_CODE = L_SPECIAL_NOTE
            UNION
               SELECT '#####' CAT_CODE FROM DUAL
               );

         IF (L_STRING != P12) THEN
            DATA_LOOKS_GOOD := DATA_ISSUE_33(12, Trim(SubStr(Nvl(P12, 'NULL'), 1, 5)));
         ELSE
            DATA_LOOKS_GOOD := TRUE;
         END IF;
      END IF;
   END PROC_RHRCOMM_CAT_CK;

   PROCEDURE PROC_UNM_ID_CK( P2 IN VARCHAR2, DATA_LOOKS_GOOD IN OUT BOOLEAN )
   IS
      L_UNM_ID                   VARCHAR2( 11 CHAR);
   BEGIN
      L_UNM_ID := Nvl(P2, 'NULL');
      IF Length(L_UNM_ID) != 9 THEN
         DATA_LOOKS_GOOD := DATA_ISSUE_33(2, L_UNM_ID);
      ELSE
         SELECT Max(SPRIDEN_ID) INTO L_UNM_ID FROM SPRIDEN WHERE SPRIDEN_ID = P2;
         IF L_UNM_ID != P2 THEN
            DATA_LOOKS_GOOD := DATA_ISSUE_33(2, L_UNM_ID);
         ELSE
            DATA_LOOKS_GOOD := TRUE;
         END IF;
      END IF;
   END PROC_UNM_ID_CK;

   PROCEDURE PROC_AIDY_CK( P1 IN VARCHAR2, AIDY_OK IN OUT BOOLEAN, DATA_LOOKS_GOOD IN OUT BOOLEAN )
   IS
   BEGIN
      L_AIDY := Nvl(P1, 'N/A');
      IF ((Length(L_AIDY) != 4) OR (UNM.RZKFLIB.IS_NUMERIC(P1) = FALSE)) THEN
         DATA_LOOKS_GOOD := DATA_ISSUE_33(1, Nvl(P1, 'NULL'));
      ELSE
         SELECT Max(RZBSCAM_AIDY_CODE) INTO L_AIDY FROM RZBSCAM WHERE RZBSCAM_AIDY_CODE = P1;
         IF ((L_AIDY != P1) OR (CURRENT_AIDY > P1)) THEN
            DATA_LOOKS_GOOD := DATA_ISSUE_33(1, Nvl(P1, 'NULL'));
         ELSE
            DATA_LOOKS_GOOD := TRUE;
            L_AIDY  := P1;
            AIDY_OK := TRUE;
         END IF;
      END IF;
      IF (L_AIDY = 'N/A') THEN
         L_AIDY := 'NULL';
      END IF;
   END PROC_AIDY_CK;

   FUNCTION THIS_IS_A_FUND_EXCEPTION( P1 IN VARCHAR2, P3 IN VARCHAR2, E1 OUT NUMBER,
                                      DATA_LOOKS_GOOD IN OUT BOOLEAN ) RETURN BOOLEAN
   IS
      L_SPECIAL_NOTE             VARCHAR2( 25 CHAR);
      L_FUND                     VARCHAR2(  6 CHAR);
   BEGIN
      E1 := 0;
      L_SPECIAL_NOTE := Upper(Nvl(P1, 'N/A'));
      IF (L_SPECIAL_NOTE = 'FUNDEXCEPTION') THEN
         FUND_ECPTN_CNT := FUND_ECPTN_CNT + 1;
         L_FUND := Upper(Nvl(P3, 'NULL'));
         SELECT Max(FUND) FUND_CODE INTO L_FUND
         FROM (SELECT RFRBASE_FUND_CODE FUND FROM RFRBASE WHERE RFRBASE_FUND_CODE = L_FUND
            UNION
               SELECT '#####' FUND FROM DUAL);

         IF (L_FUND = P3) THEN
            SP_FUND_COUNT := SP_FUND_COUNT + 1;
            SPECIAL_FUND(SP_FUND_COUNT) := L_FUND;
         ELSE
            DATA_LOOKS_GOOD := DATA_ISSUE_33(15, Nvl(P3, 'NULL'));
            E1 := 1;
         END IF;
         L_ERROR_FIELD := 0;
         RETURN TRUE;
      ELSE
         RETURN FALSE;
      END IF;
   END THIS_IS_A_FUND_EXCEPTION;

PROCEDURE SHOW_MESSAGE_1( STRING IN VARCHAR2, FLAG IN BOOLEAN )
IS
BEGIN
   IF FLAG THEN
      Dbms_Output.Put_Line( STRING||'TRUE' );
   ELSE
      Dbms_Output.Put_Line( STRING||'FALSE' );
   END IF;
END SHOW_MESSAGE_1;

/*===================================================================================================
== UNM.RZRSCIN ~~~ Scholarship Tool Input Parameter Data
==    RZRSCIN_... (fields)
==    ...AIDY_CODE     ~~~ AID Year Code: The aid year associated with the information in this record.
==    ...UNM_ID        ~~~ UNM_ID: Internal system-generated student identification number.
==    ...FUND_CODE     ~~~ Fund Code: The Financial Aid code for the award processing.
==    ...TERM_CODE     ~~~ [optional] Term (Period) Code: The school period associated with the information in this record.
==    ...AWARD_AMT     ~~~ Award Amount: The Financial Aid monetary amount for the award processing.
==    ...AWST_CODE     ~~~ Award Action: The code (ACCP or CANC) for the award processing.
==    ...AWRD_LTR_IND  ~~~ [optional] AWARD LETTER INDICATOR: Indicates whether an award letter should be generated for the applicant.
==    ...PCKG_LOAD_IND ~~~ [optional] PACKAGING LOAD INDICATOR: The enrollment status for this period.
==    ...RRRAREQ_TREQ  ~~~ [optional] RRRAREQ TREQ Code: The code associated with the tracking requirement.';
==    ...RRRAREQ_TRST  ~~~ [optional] RRRAREQ TRST Code: The status of the tracking requirement.';
==    ...RRRAREQ_PRD   ~~~ [optional] RRRAREQ PERIOD: The period associated with the requirement; valid only if RTVTREQ_TERM_ELIGIBLE_IND = "Y"';
==    ...RHRCOMM_Cat   ~~~ [optional] RHRCOMM Category: The category associated with the RHRCOMM comment.
==    ...RHRCOMM_Txt   ~~~ [optional] RHRCOMM Text: The text of the RHRCOMM comment.
==    ...RORMESG_EXPR  ~~~ [optional] EXPIRATION DATE: Date the message expires.
==    ...RORMESG_CODE  ~~~ [optional] RORMESG Code: The code associated with the RORMESG comment.
==    ...RORMESG_Txt   ~~~ [optional] RORMESG Full Text: The text of the RORMESG comment(s).
===================================================================================================
==  Improper to try using "FUNDEXCEPTION" and "COMMENTONLY" in same data line
==   field( 1) is AIDY_CODE     ~~~ Required: AIDY or "FUNDEXCEPTION"; AIDY must be valid and not in past
==   field( 2) is UNM_ID        ~~~ Required: except when f1 = "FUNDEXCEPTION"
==   field( 3) is FUND_CODE     ~~~ Required: Fund Code or "COMMENTONLY"; Fund Code must be an exception or validated against RZBSCAM
==   field( 4) is TERM_CODE     ~~~ Optional for Award: Term must be valid, paired with AIDY,and not in past
==   field( 5) is AWARD_AMT     ~~~ Required for Award: except when f1 = "FUNDEXCEPTION" or when f3 = "COMMENTONLY"
==   field( 6) is AWST_CODE     ~~~ Required for Award: except when f1 = "FUNDEXCEPTION" or when f3 = "COMMENTONLY"
==   field( 7) is AWRD_LTR_IND  ~~~ [optional] to override DEFAULT of 'Y' with 'N'
==   field( 8) is PCKG_LOAD_IND ~~~ [optional] to override DEFAULT of 1 with a # (range [1..5])
==   L_fld( 9) is RRRAREQ_TREQ_CODE  ~~~ [optional] RRRAREQ TREQ Code: The code associated with the tracking requirement.';
==   L_fld(10) is RRRAREQ_TRST_CODE  ~~~ [optional] RRRAREQ TRST Code: The status of the tracking requirement.';
==   L_fld(11) is RRRAREQ_PERIOD     ~~~ [optional] RRRAREQ PERIOD: The period associated with the requirement; valid only if RTVTREQ_TERM_ELIGIBLE_IND = "Y"';
==   L_fld(12) is RHRCOMM_Cat   ~~~ [optional] if present, validated against RTVCCOM
==   L_fld(13) is RHRCOMM_Txt   ~~~ [optional]
==   L_fld(14) is RORMESG_EXPR  ~~~ [optional] to override DEFAULT of 30-Days with # of Days (> 0)
==   L_fld(15) is RORMESG_CODE  ~~~ [optional] if present, validated against RTVMESG
==   L_fld(16) is RORMESG_Txt   ~~~ [optional]
===================================================================================================*/
   FUNCTION F_VERIFY_INPUT_DATA  RETURN NUMBER
   IS
      E1                         NUMBER  := 0;
      DATA_LOOKS_GOOD            BOOLEAN := TRUE;
      DATA_ISSUES                NUMBER  := 0;
      AIDY_OK                    BOOLEAN := FALSE;
      L_SPECIAL_NOTE             VARCHAR2( 25 CHAR);
   BEGIN
      L_SPECIAL_NOTE := Upper(Nvl(L_FLD(3), 'N/A'));
      L_ERROR_FIELD := 0;
      IF THIS_IS_A_FUND_EXCEPTION( L_FLD(1), L_FLD(3), E1, DATA_LOOKS_GOOD ) THEN
         IF (DATA_LOOKS_GOOD = FALSE) THEN DATA_ISSUES := DATA_ISSUES + 1; END IF;
         RETURN E1;
      ELSIF (L_SPECIAL_NOTE = 'COMMENTONLY') THEN
         RETURN DATA_ISSUES;
      ELSE
         L_ERROR_FIELD := 1;
         PROC_AIDY_CK( L_FLD(1), AIDY_OK, DATA_LOOKS_GOOD );
         IF (DATA_LOOKS_GOOD = FALSE) THEN DATA_ISSUES := DATA_ISSUES + 1; END IF;
         L_ERROR_FIELD := 2;
         PROC_UNM_ID_CK( L_FLD(2), DATA_LOOKS_GOOD );
         IF (DATA_LOOKS_GOOD = FALSE) THEN DATA_ISSUES := DATA_ISSUES + 1; END IF;
         L_ERROR_FIELD := 3;
         PROC_FUND_CODE_CK( L_FLD(3), DATA_LOOKS_GOOD );
         IF (DATA_LOOKS_GOOD = FALSE) THEN DATA_ISSUES := DATA_ISSUES + 1; END IF;
         L_ERROR_FIELD := 4;
         PROC_TERM_CODE_CK( L_FLD(1), AIDY_OK, L_FLD(4), DATA_LOOKS_GOOD );
         IF (DATA_LOOKS_GOOD = FALSE) THEN DATA_ISSUES := DATA_ISSUES + 1; END IF;
         L_ERROR_FIELD := 5;
         PROC_AMOUNT_CK( L_FLD(5), DATA_LOOKS_GOOD );
         IF (DATA_LOOKS_GOOD = FALSE) THEN DATA_ISSUES := DATA_ISSUES + 1; END IF;
         L_ERROR_FIELD := 6;
         PROC_ACTION_CK( L_FLD(6), DATA_LOOKS_GOOD );
         IF (DATA_LOOKS_GOOD = FALSE) THEN DATA_ISSUES := DATA_ISSUES + 1; END IF;
         IF (FIELD_COUNT > 6) THEN
            L_ERROR_FIELD := 7;
            PROC_AWRD_LTR_CK( L_FLD(7), DATA_LOOKS_GOOD );
            IF (DATA_LOOKS_GOOD = FALSE) THEN DATA_ISSUES := DATA_ISSUES + 1; END IF;
         END IF;
         IF (FIELD_COUNT > 7) THEN
            L_ERROR_FIELD := 8;
            PROC_ACAD_LD_CK( L_FLD(8), DATA_LOOKS_GOOD, G_ACADEMIC_LOAD );
            IF (DATA_LOOKS_GOOD = FALSE) THEN DATA_ISSUES := DATA_ISSUES + 1; END IF;
         END IF;
         G_RRRAREQ_TREQ_CODE := 'x';
         G_RRRAREQ_TRST_CODE := 'x';
         INDX := 9;
         IF (FIELD_COUNT >= INDX) THEN
            G_RRRAREQ_TREQ_CODE := Nvl(L_FLD(INDX), 'x');
            INDX := INDX + 1;
            IF (FIELD_COUNT >= INDX) THEN
               G_RRRAREQ_TRST_CODE := Nvl(L_FLD(INDX), 'x');
               INDX := INDX + 1;
               IF (FIELD_COUNT >= INDX) THEN
                  G_RRRAREQ_PERIOD := L_FLD(INDX);
               END IF;
            END IF;
            G_AWARD_AIDY  := L_FLD(1);
            G_UNM_BANID   := L_FLD(2);
            G_FUND_CODE   := L_FLD(3);
            G_BANNER_PIDM := GB_COMMON.F_GET_PIDM( G_UNM_BANID );
            ANALYSIS_CODE := 0;
            IF ((G_RRRAREQ_TREQ_CODE != 'x') AND (G_RRRAREQ_TRST_CODE != 'x')) THEN
               ANALYSIS_CODE := UNM.RZKFLIB.F_RRRAREQ_DATA_OK( ANALYSIS_NOTE, G_BANNER_PIDM, G_AWARD_AIDY, G_FUND_CODE, G_RRRAREQ_TREQ_CODE, G_RRRAREQ_TRST_CODE, G_RRRAREQ_PERIOD );
               IF (ANALYSIS_CODE < 1) THEN
                  IF (ANALYSIS_CODE = 0) THEN
                     ANALYSIS_NOTE := G_RRRAREQ_TREQ_CODE;
                     DATA_IS_OK    := DATA_ISSUE_33(9, ANALYSIS_NOTE );
                  ELSIF (ANALYSIS_CODE = -1) THEN
                     ANALYSIS_NOTE := G_RRRAREQ_PERIOD;
                     DATA_IS_OK    := DATA_ISSUE_33(10, ANALYSIS_NOTE );
                  ELSIF (ANALYSIS_CODE = -2) THEN
                     ANALYSIS_NOTE := G_RRRAREQ_TRST_CODE;
                     DATA_IS_OK    := DATA_ISSUE_33(11, ANALYSIS_NOTE );
                  ELSIF (ANALYSIS_CODE = -3) THEN
                     ANALYSIS_NOTE := 'NULL';
                     DATA_IS_OK    := DATA_ISSUE_33(11, ANALYSIS_NOTE );
                  END IF;
                  DATA_ISSUES   := DATA_ISSUES + 1;
                  DATA_LOOKS_GOOD := FALSE;
               END IF;
            ELSIF ((G_RRRAREQ_TREQ_CODE != 'x') AND (G_RRRAREQ_TRST_CODE = 'x')) THEN
               ANALYSIS_NOTE := 'NULL';
               DATA_IS_OK    := DATA_ISSUE_33(11, ANALYSIS_NOTE );
               DATA_ISSUES   := DATA_ISSUES + 1;
               DATA_LOOKS_GOOD := FALSE;
            END IF;
         END IF;
         IF (FIELD_COUNT > 11) THEN
            L_ERROR_FIELD := 12;
            PROC_RHRCOMM_CAT_CK( L_FLD(12), DATA_LOOKS_GOOD );
            IF (DATA_LOOKS_GOOD = FALSE) THEN DATA_ISSUES := DATA_ISSUES + 1; END IF;
            IF (FIELD_COUNT > 13) THEN
               L_ERROR_FIELD := 14;
               PROC_RORMESG_DATE_CK( L_FLD(14), DATA_LOOKS_GOOD, G_RORMESG_DATE_PERIOD );
               IF (DATA_LOOKS_GOOD = FALSE) THEN DATA_ISSUES := DATA_ISSUES + 1; END IF;
               IF (FIELD_COUNT > 14) THEN
                  L_ERROR_FIELD := 15;
                  PROC_RORMESG_CAT_CK( L_FLD(15), DATA_LOOKS_GOOD );
                  IF (DATA_LOOKS_GOOD = FALSE) THEN DATA_ISSUES := DATA_ISSUES + 1; END IF;
               END IF;
            END IF;
         END IF;
      END IF;  -- end of: IF This_Is_A_Fund_Exception(...) THEN ... ELSE

      IF DATA_LOOKS_GOOD THEN
         IF ((L_AIDY = L_FLD(1)) AND (L_TERM = L_FLD(4))) THEN
            TERM_AWARD_COUNT := TERM_AWARD_COUNT + 1;
         ELSIF ((L_AIDY = L_FLD(1)) AND (L_TERM = 'NULL')) THEN
            AIDY_AWARD_COUNT := AIDY_AWARD_COUNT + 1;
         END IF;
      END IF;

      L_ERROR_FIELD := 0;
      RETURN DATA_ISSUES;

   EXCEPTION
      WHEN OTHERS THEN
         Dbms_Output.Put_Line( 'f_Verify_Input_Data() ==> L_ERROR_FIELD: '||L_ERROR_FIELD||'   ...Field_Count: '||FIELD_COUNT );
         UNM.RZKFLIB.TRACK_ERRORS;
         IF ((L_ERROR_FIELD >= 1) AND (L_ERROR_FIELD <= 13))THEN
            APPEND_ERROR_NOTE(L_FLD_ERRDATA(L_ERROR_FIELD), Nvl(L_FLD(L_ERROR_FIELD), 'NULL'), L_FLD_UNQERR(L_ERROR_FIELD));
         ELSIF (L_ERROR_FIELD = 14)THEN
            APPEND_ERROR_NOTE(L_FLD_ERRDATA(L_ERROR_FIELD), L_FLD(1)||'~'||L_FLD(4), L_FLD_UNQERR(L_ERROR_FIELD));
         END IF;
         IF DATA_LOOKS_GOOD THEN
            IF ((L_AIDY = L_FLD(1)) AND (L_TERM = L_FLD(4))) THEN
               TERM_AWARD_COUNT := TERM_AWARD_COUNT + 1;
            ELSIF ((L_AIDY = L_FLD(1)) AND (L_TERM = 'NULL')) THEN
               AIDY_AWARD_COUNT := AIDY_AWARD_COUNT + 1;
            END IF;
         END IF;

         RETURN DATA_ISSUES;
   END F_VERIFY_INPUT_DATA;

   FUNCTION FMT_INT( NUMB IN NUMBER, NMX IN NUMBER )
      RETURN VARCHAR2
   IS
      FMTD_INTGR        VARCHAR2( 15 CHAR );
      FORMAT_MASK       VARCHAR2( 15 CHAR ) := '999,999,990';
      MAX_DIGITS        NUMBER;
      I                 NUMBER;

   BEGIN
      MAX_DIGITS  := Length(Trim(To_Char(NMX, '9999999')));
      FORMAT_MASK := '0';
      FOR I IN 2..MAX_DIGITS LOOP
         IF (Mod(I, 4) = 0) THEN
            FORMAT_MASK := ','||FORMAT_MASK;
         END IF;
         FORMAT_MASK := '9'||FORMAT_MASK;
      END LOOP;

      FMTD_INTGR := To_Char( NUMB, FORMAT_MASK );
      RETURN FMTD_INTGR;
   END FMT_INT;

   FUNCTION EXTRACT_AWARDING_DATA( DOWNLOAD_DATA IN VARCHAR2 )
      RETURN VARCHAR2
   IS
      TC          NUMBER := 0;
      TP          NUMBER := 0;
   BEGIN
      FOR I IN 1..Length(DOWNLOAD_DATA) LOOP
         IF (SubStr(DOWNLOAD_DATA, I, 1) = CHR(9)) THEN
            TC := TC + 1;
            TP := I;
         EXIT WHEN (TC > 5);
         END IF;
      END LOOP;
      IF (TC > 5) THEN
         RETURN SubStr(DOWNLOAD_DATA, 1, TP-1);
      ELSE
         RETURN DOWNLOAD_DATA;
      END IF;
   END EXTRACT_AWARDING_DATA;

   FUNCTION F_GET_PIDM( UNM_ID IN SPRIDEN.SPRIDEN_ID%TYPE )
         RETURN SPRIDEN.SPRIDEN_PIDM%TYPE
      IS
         V_RETURN   NUMBER;
      BEGIN
         V_RETURN := GB_COMMON.F_GET_PIDM( UNM_ID );
         RETURN V_RETURN;

      EXCEPTION
         WHEN OTHERS THEN
            V_RETURN := NULL;
            RETURN V_RETURN;
   END F_GET_PIDM;

   FUNCTION RTRIM_CTRL( STRING IN VARCHAR2 ) RETURN VARCHAR2
   IS
      RTN_VALUE      VARCHAR2(1000 CHAR);
      IDX            NUMBER;
   BEGIN
      IF STRING IS NULL THEN
         RTN_VALUE := NULL;
      ELSIF (Length(STRING) = 0) THEN
         RTN_VALUE := STRING;
      ELSE
-- Dbms_Output.Put_Line( 'string ==>'||string||'<<' );
         RTN_VALUE := '';
         IDX  := Length(STRING);
         FOR I IN 0..Length(STRING) LOOP
            IF Ascii(SubStr(STRING, IDX-I, 1)) >= 32 THEN
               RTN_VALUE := SubStr(STRING, 1, IDX-I);
               EXIT;
            END IF;
         END LOOP;
         RTN_VALUE := RTrim(RTN_VALUE);
-- Dbms_Output.Put_Line( 'now at ==>'||rtn_value||'<<' );
      END IF;
      RETURN RTN_VALUE;
   END RTRIM_CTRL;

BEGIN
   IF BANNER_ONE_UP_NUM IS NULL THEN
      SELECT To_Number(To_Char(SYSDATE, 'DHH24MISS'))
      INTO ONE_UP_NUMBER
      FROM DUAL;
   ELSIF (BANNER_ONE_UP_NUM < 0) THEN
      INTERNAL_VERSION := F_API_VERSION;
      RETURN INTERNAL_VERSION;
   ELSE
      ONE_UP_NUMBER := BANNER_ONE_UP_NUM;
   END IF;

   SELECT NAME
   INTO INSTANCE_NAME
   FROM V$DATABASE;

   SELECT Upper(SubStr(USERNAME, 1, 50))
   INTO USER_NAME
   FROM V$SESSION S
   WHERE USERNAME IS NOT NULL
     AND AUDSID = UserEnv('sessionid');

   CURRENT_AIDY := RZKFLIB.THE_PERIOD_TODAY('AIDY');
   CURRENT_TERM := RZKFLIB.THE_PERIOD_TODAY('TERM');

   <<USER_NAME_OK_BLOCK>>
   BEGIN
      IF (USER_NAME != 'FINAIDAPPWORX') THEN
         G_BASE_FILENAME  := 'SFAO_SCH_LOAD_'||USER_NAME;
         G_INPUT_FILENAME := G_BASE_FILENAME ||G_FILE_EXTENTION;
         SELECT Count(*) INTO CNT FROM RZRSCIN;
         ROW_COUNT := CNT;
         INTERNAL_VERSION := F_API_VERSION;
         TABLE_INSERT_COUNT := 0;
      END IF;

      Dbms_Output.new_line;
      Dbms_Output.put_line( '=======================================================');
      Dbms_Output.put_line( 'Load Scholarship Parameters; Version: '||INTERNAL_VERSION );
      Dbms_Output.put_line( '...Executed by: ' || USER_NAME || ' in dB Instace: ' || INSTANCE_NAME );
      IF (USER_NAME = 'FINAIDAPPWORX') THEN
         UNATHORIZED_USER := 1;
         Dbms_Output.put_line( '   "FINAIDAPPWORX" Not Authorized to Run this Program.' );
         Dbms_Output.Put_Line( '       ...No further processing.' );
      ELSE
         Dbms_Output.put_line( '   ...The INPUT Filename is '||G_INPUT_FILENAME         );
         IF (ROW_COUNT = 1) THEN
            Dbms_Output.put_line( '   ...There is 1 row of data in RZRSCIN'  );
         ELSE
            Dbms_Output.put_line( '   ...There are ' || ROW_COUNT || ' rows of data in RZRSCIN'  );
         END IF;
         Dbms_Output.put_line( '   ...Current_AIDY: '||CURRENT_AIDY||' and Current_TERM: '||CURRENT_TERM );
      END IF;
      Dbms_Output.put_line( '=======================================================');
      Dbms_Output.new_line;

      G_MESSAGE_BODY := '=======================================================';
      G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'Load Scholarship Parameters; Version: '||INTERNAL_VERSION ;
      G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'...Executed by: ' || USER_NAME || ' in dB Instance: ' || INSTANCE_NAME;
      G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'...Executed on: ' || To_Char(SYSDATE, 'MM-DD-YYYY')||' at '||To_Char(SYSDATE, 'HH24:MI:SS');
      IF (USER_NAME = 'FINAIDAPPWORX') THEN
         G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'   "FINAIDAPPWORX" Not Authorized to Run this Program.';
         G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'       ...No further processing.';
      ELSE
         G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'   ...The INPUT Filename is '||G_INPUT_FILENAME;
         IF (ROW_COUNT = 1) THEN
            G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'   ...There is 1 row of data in RZRSCIN';
         ELSE
            G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'   ...There are ' || ROW_COUNT || ' rows of data in RZRSCIN';
         END IF;
         G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'   ...Current_AIDY: '||CURRENT_AIDY||' and Current_TERM: '||CURRENT_TERM;
      END IF;
      G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'======================================================='||CRLF;

      <<READ_DATA_BLOCK>>
      BEGIN
         IF (USER_NAME != 'FINAIDAPPWORX') THEN
            Utl_File.FGETATTR( G_FILE_DIRECTORY, G_INPUT_FILENAME, EXISTS_FLAG, FILE_LENGTH, BLOCKSIZE );
            IF (EXISTS_FLAG = TRUE) THEN
               DATA_IMPORT_FILE_EXISTS := TRUE;
            ELSE
               Dbms_Output.Put_Line( ' Scholarship Import Data file does not exist. No further processing.' );
               Dbms_Output.Put_Line( '   File_Directory: '||  G_FILE_DIRECTORY );
               Dbms_Output.Put_Line( '  Input_File_Name: '||  G_INPUT_FILENAME );
               Dbms_Output.Put_Line( '      exists_flag: '||           'FALSE' );
               G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||' Scholarship Import Data file does not exist. No further processing.';
               G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'~~~  ~~~  ~~~  ~~~  ~~~  ~~~  ~~~  ~~~';
               G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'   File_Directory: '||  G_FILE_DIRECTORY;
               G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||' Input_File_Name: '||  G_INPUT_FILENAME ;
               G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'      exists_flag: '||           'FALSE';
               G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'======================================================='||CRLF;
               DATA_IMPORT_FILE_EXISTS := FALSE;
            END IF;

            IF DATA_IMPORT_FILE_EXISTS THEN
               G_INPUT_HANDLE  := Utl_File.FOPEN(G_FILE_DIRECTORY, G_INPUT_FILENAME,   'R');
               FILE_READ_COUNT := 0;
               TOTAL_FIELD_ERRORS := 0;
               AIDY_AWARD_COUNT   := 0;
               TERM_AWARD_COUNT   := 0;
               CHAR_TRKR_CNT      := 0;
               CHAR_TRKR_ROW_CNT  := 0;
               FOR N IN 1..MAX_FIELDS LOOP
                  L_FLD(N)         := '';
                  L_FLD_DESC(N)    := '';
                  L_FLD_ERRDATA(N) := '';
                  L_FLD_ERR(N)     := 0;
                  L_FLD_UNQERR(N)  := 0;
               END LOOP;
               L_FLD_DESC( 1) := ' AIDY Code column...........:';     L_FLD_MSG_ORDER( 1) := 19;
               L_FLD_DESC( 2) := ' UNM_ID column..............:';     L_FLD_MSG_ORDER( 2) :=  1;
               L_FLD_DESC( 3) := ' FUND Code column...........:';     L_FLD_MSG_ORDER( 3) :=  2;
               L_FLD_DESC( 4) := ' TERM Code column...........:';     L_FLD_MSG_ORDER( 4) :=  3;
--                L_fld_desc( 5) := ' AMOUNT column..............:';     L_fld_MSG_Order( 5) :=  4;
               L_FLD_DESC( 5) := ' AMOUNT non-Numeric.........:';     L_FLD_MSG_ORDER( 5) :=  4;
               L_FLD_DESC(19) := ' AMOUNT Exceeds Fund Max-Min:';     L_FLD_MSG_ORDER(19) :=  5;
               L_FLD_DESC( 6) := ' ACTION column..............:';     L_FLD_MSG_ORDER( 6) :=  6;
               L_FLD_DESC( 7) := ' AWRD Letter Indicator......:';     L_FLD_MSG_ORDER( 7) :=  7;
               L_FLD_DESC( 8) := ' Academic Load..............:';     L_FLD_MSG_ORDER( 8) :=  8;
               L_FLD_DESC( 9) := ' RRRAREQ Period Ineligible..:';     L_FLD_MSG_ORDER( 9) :=  9;
               L_FLD_DESC(10) := ' RRRAREQ Period Invalid.....:';     L_FLD_MSG_ORDER(10) := 10;
               L_FLD_DESC(11) := ' RRRAREQ TRST Issue.........:';     L_FLD_MSG_ORDER(11) := 11;
               L_FLD_DESC(12) := ' RHRCOMM Category Code......:';     L_FLD_MSG_ORDER(12) := 12;
               L_FLD_DESC(13) := ' RHRCOMM Message............:';     L_FLD_MSG_ORDER(13) := 13;
               L_FLD_DESC(14) := ' RORMESG Period.............:';     L_FLD_MSG_ORDER(14) := 14;
               L_FLD_DESC(15) := ' RORMESG Message Code.......:';     L_FLD_MSG_ORDER(15) := 15;
               L_FLD_DESC(16) := ' RORMESG Message............:';     L_FLD_MSG_ORDER(16) := 16;
               L_FLD_DESC(17) := ' AIDY~TERM use..............:';     L_FLD_MSG_ORDER(17) := 17;
               L_FLD_DESC(18) := ' FUND Exception.............:';     L_FLD_MSG_ORDER(18) := 18;
               FOR N IN 1..CHAR_TRKR_MAX_CNT LOOP
                  CHAR_TRKR(N) := 0;
               END LOOP;
--==============================================================================
--==============================================================================
   IF (INSTANCE_NAME = 'DEVL') THEN
      G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF;
      G_MESSAGE_BODY := G_MESSAGE_BODY||'~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~'||CRLF;
      G_MESSAGE_BODY := G_MESSAGE_BODY||'~~~~~  Input data displayed (DEVL Only) for reference   ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~'||CRLF;
      G_MESSAGE_BODY := G_MESSAGE_BODY||'~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~';
   END IF;
--==============================================================================
--==============================================================================
               LOOP
                  <<DATA_ROW_COUNT_BLOCK>>
                  BEGIN
                     Utl_File.GET_LINE(G_INPUT_HANDLE, DOWNLOAD_DATA);
                     DOWNLOAD_DATA := RTRIM_CTRL( DOWNLOAD_DATA );
/*
                     n := Length(Download_Data);
                     IF (n > 1) THEN
                        IF (ASCII(SubStr(Download_Data, 1, n)) < 32) THEN
                           Download_Data := SubStr(Download_Data, 1, n-1);
                        END IF;
                     END IF;
*/
                     TMPNUMBER := Nvl(Length(REPLACE(DOWNLOAD_DATA, Chr(9), '')), 0);
                     <<DATA_READ_NOT_BLANK_BLOCK>>
                     BEGIN
                        IF (TMPNUMBER > 1) THEN
                           DOWNLOAD_DATA   := Trim(DOWNLOAD_DATA);
                           FILE_READ_COUNT := FILE_READ_COUNT + 1;
                           AWARDING_DATA := EXTRACT_AWARDING_DATA( DOWNLOAD_DATA );
                           IF (UNALLOWED_CHARACTER_INPUT( AWARDING_DATA ) = TRUE) THEN
                              TOTAL_FIELD_ERRORS := TOTAL_FIELD_ERRORS + 1;
                           ELSE
/*===================================================================================================
== UNM.RZRSCIN ~~~ Scholarship Tool Input Parameter Data
==    RZRSCIN_... (fields)
==    ...AIDY_CODE     ~~~ AID Year Code: The aid year associated with the information in this record.
==    ...UNM_ID        ~~~ UNM_ID: Internal system-generated student identification number.
==    ...FUND_CODE     ~~~ Fund Code: The Financial Aid code for the award processing.
==    ...TERM_CODE     ~~~ [optional] Term (Period) Code: The school period associated with the information in this record.
==    ...AWARD_AMT     ~~~ Award Amount: The Financial Aid monetary amount for the award processing.
==    ...AWST_CODE     ~~~ Award Action: The code (ACCP or CANC) for the award processing.
==    ...AWRD_LTR_IND  ~~~ [optional] AWARD LETTER INDICATOR: Indicates whether an award letter should be generated for the applicant.
==    ...PCKG_LOAD_IND ~~~ [optional] PACKAGING LOAD INDICATOR: The enrollment status for this period.
==    ...RRRAREQ_TREQ  ~~~ [optional] RRRAREQ TREQ Code: The code associated with the tracking requirement.';
==    ...RRRAREQ_TRST  ~~~ [optional] RRRAREQ TRST Code: The status of the tracking requirement.';
==    ...RRRAREQ_PRD   ~~~ [optional] RRRAREQ PERIOD: The period associated with the requirement; valid only if RTVTREQ_TERM_ELIGIBLE_IND = "Y"';
==    ...RHRCOMM_Cat   ~~~ [optional] RHRCOMM Category: The category associated with the RHRCOMM comment.
==    ...RHRCOMM_Txt   ~~~ [optional] RHRCOMM Text: The text of the RHRCOMM comment.
==    ...RORMESG_EXPR  ~~~ [optional] EXPIRATION DATE: Date the message expires.
==    ...RORMESG_CODE  ~~~ [optional] RORMESG Code: The code associated with the RORMESG comment.
==    ...RORMESG_Txt   ~~~ [optional] RORMESG Full Text: The text of the RORMESG comment(s).
===================================================================================================
==  Improper to try using "FUNDEXCEPTION" and "COMMENTONLY" in same data line
==   L_fld( 1) is AIDY_CODE     ~~~ Required: AIDY or "FUNDEXCEPTION"; AIDY must be valid and not in past
==   L_fld( 2) is UNM_ID        ~~~ Required: except when f1 = "FUNDEXCEPTION"
==   L_fld( 3) is FUND_CODE     ~~~ Required: Fund Code or "COMMENTONLY"; Fund Code must be an exception or validated against RZBSCAM
==   L_fld( 4) is TERM_CODE     ~~~ Optional for Award: Term must be valid, paired with AIDY,and not in past
==   L_fld( 5) is AWARD_AMT     ~~~ Required for Award: except when f1 = "FUNDEXCEPTION" or when f3 = "COMMENTONLY"
==   L_fld( 6) is AWST_CODE     ~~~ Required for Award: except when f1 = "FUNDEXCEPTION" or when f3 = "COMMENTONLY"
==   L_fld( 7) is AWRD_LTR_IND  ~~~ [optional] to override DEFAULT of 'Y' with 'N'
==   L_fld( 8) is PCKG_LOAD_IND ~~~ [optional] to override DEFAULT of 1 with a # (range [1..5])
==   L_fld( 9) is RRRAREQ_TREQ_CODE  ~~~ [optional] RRRAREQ TREQ Code: The code associated with the tracking requirement.';
==   L_fld(10) is RRRAREQ_TRST_CODE  ~~~ [optional] RRRAREQ TRST Code: The status of the tracking requirement.';
==   L_fld(11) is RRRAREQ_PERIOD     ~~~ [optional] RRRAREQ PERIOD: The period associated with the requirement; valid only if RTVTREQ_TERM_ELIGIBLE_IND = "Y"';
==   L_fld(12) is RHRCOMM_Cat   ~~~ [optional] if present, validated against RTVCCOM
==   L_fld(13) is RHRCOMM_Txt   ~~~ [optional]
==   L_fld(14) is RORMESG_EXPR  ~~~ [optional] to override DEFAULT of 30-Days with # of Days (> 0)
==   L_fld(15) is RORMESG_CODE  ~~~ [optional] if present, validated against RTVMESG
==   L_fld(16) is RORMESG_Txt   ~~~ [optional]
===================================================================================================*/
                              -- Initialize the data fields...
                              G_UNM_BANID         := NULL;
                              G_AWARD_AIDY        := NULL;
                              G_AWARD_TERM        := NULL;
                              G_FUND_CODE         := NULL;
                              G_AWARD_AMOUNT      := NULL;
                              G_AWARD_ACTION      := NULL;
                              G_AWRD_LTR_IND      := NULL;
                              G_ACADEMIC_LOAD     := NULL;
                              G_RHRCOMM_CAT_CODE  := NULL;
                              G_TEXT_FOR_RHRCOMM  := NULL;
                              G_EXDY_FOR_RORMESG  := NULL;
                              G_CODE_FOR_RORMESG  := NULL;
                              G_TEXT_FOR_RORMESG  := NULL;
                              G_RRRAREQ_TREQ_CODE := NULL;
                              G_RRRAREQ_TRST_CODE := NULL;
                              G_RRRAREQ_PERIOD    := NULL;
                              L_FLD( 1) := NULL;
                              L_FLD( 2) := NULL;
                              L_FLD( 3) := NULL;
                              L_FLD( 4) := NULL;
                              L_FLD( 5) := NULL;
                              L_FLD( 6) := NULL;
                              L_FLD( 7) := NULL;
                              L_FLD( 8) := NULL;
                              L_FLD( 9) := NULL;
                              L_FLD(10) := NULL;
                              L_FLD(11) := NULL;
                              L_FLD(12) := NULL;
                              L_FLD(13) := NULL;
                              L_FLD(14) := NULL;
                              L_FLD(15) := NULL;
                              L_FLD := UNM.ARRAYTABLES.F_PARSE_STRING( DOWNLOAD_DATA, HT, FIELD_COUNT );
-- FOR h IN 1..Field_Count LOOP
--    Dbms_Output.Put_Line( '~~~~~['||Field_Count||']~~~~~ L_fld('||To_Char(h, '90')||') ==>'||SubStr(L_fld(h), 1, 35)||'<==' );
-- END LOOP;

                              TOTAL_FIELD_ERRORS := TOTAL_FIELD_ERRORS + F_VERIFY_INPUT_DATA;
-- -- -- Dbms_Output.New_Line;

                              /*====================================================================================
                              == RHRCOMM is for Applicants Comments
                              ======================================================================================
                              ==                                             DATA                       NULL- COL ==
                              == OWNER       COLUMN_NAME           DATA_TYPE LENGTH DATA Descriptions    ABLE  ID ==
                              == =======     ===================== ========= ====== =================== ===== === ==
                              == FAISMGR     RHRCOMM_PIDM          NUMBER        22 NUMBER   (   8)        N    1 ==
                              ==             RHRCOMM_SEQNO         NUMBER        22 NUMBER   (  22)        N    2 ==
                              == TABLE_NAME  RHRCOMM_USER_ID       VARCHAR2     120 VARCHAR2 (  30 CHR)    Y    3 ==
                              == ==========  RHRCOMM_ACTIVITY_DATE DATE           7 DATE     (   7)        Y    4 ==
                              ==    RHRCOMM  RHRCOMM_ORIG_DATE     DATE           7 DATE     (   7)        N    5 ==
                              ==             RHRCOMM_AIDY_CODE     VARCHAR2      16 VARCHAR2 (   4 CHR)    Y    6 ==
                              ==             RHRCOMM_COMMENT       VARCHAR2    4000 VARCHAR2 (4000 CHR)    Y    7 ==
                              ==             RHRCOMM_CATEGORY_CODE VARCHAR2      28 VARCHAR2 (   7 CHR)    Y    8 ==
                              ==             RHRCOMM_DATA_ORIGIN   VARCHAR2     120 VARCHAR2 (  30 CHR)    Y    9 ==
                              ======================================================================================
                              == DATA Descriptions
                              ======================================================================================
                              == PIDM:             System-generated student identification number.
                              == SEQUENCE NUMBER:  System-generated sequence number assigned to the comment/record.
                              == USER ID:          User ID of the person who inserted or last updated this record.
                              == ACTIVITY DATE:    Date that information in this record was entered or last updated.
                              == ORIGINAL DATE:    Date that the original comment was entered on this student record.
                              == AID YEAR CODE:    Aid year associated with the information in this record.
                              == COMMENT:          Comments associated with this student record.
                              == CATEGORY CODE:    Category code for applicant comments.
                              == DATA ORIGIN:      Source system that created or updated the data.
                              ======================================================================================*/
                              --============================================================
                              -- Keep a count of input data rows that are not for the Fund
                              -- Code rules exceptions.
                              --------------------------------------------------------------
                              TMPSTRING := Upper(Nvl(L_FLD(1), 'N/A'));
                              IF (TMPSTRING != 'FUNDEXCEPTION') THEN
                                 TOTAL_PARAM_DATA_ROWS := TOTAL_PARAM_DATA_ROWS + 1;
                                 TMPSTRING2 := Upper(Nvl(L_FLD(3), 'N/A'));
                                 IF (TMPSTRING2 != 'COMMENTONLY') THEN
                                    PARAM_DATA_ROWS_WITH_AWARD := PARAM_DATA_ROWS_WITH_AWARD + 1;
                                 ELSE
                                    TOTAL_COMMENT_ROWS := TOTAL_COMMENT_ROWS + 1;
                                 END IF;
                                 <<DATA_ROW_AWARD_BLOCK>>
                                 BEGIN
                                    IF (TMPSTRING2 != 'COMMENTONLY') THEN
                                       INDX := 7;
                                       IF (FIELD_COUNT >= INDX) THEN
                                          AWRD_LTR_IND_FLAGS := AWRD_LTR_IND_FLAGS + 1;
                                          IF (L_FLD(INDX) IS NOT NULL) THEN
                                             IF (Upper(L_FLD(INDX)) = 'Y') THEN
                                                AWRD_LTR_IND_YES := AWRD_LTR_IND_YES + 1;
                                             ELSIF (Upper(L_FLD(INDX)) = 'N') THEN
                                                AWRD_LTR_IND_NO := AWRD_LTR_IND_NO + 1;
                                             END IF;
                                          ELSE
                                             AWRD_LTR_IND_NULLS := AWRD_LTR_IND_NULLS + 1;
                                          END IF;
                                       ELSE
                                          AWRD_LTR_IND_NULLS := AWRD_LTR_IND_NULLS + 1;
                                       END IF;
                                       INDX := 8;
                                       IF (FIELD_COUNT >= INDX) THEN
                                          PROC_ACAD_LD_CK( L_FLD(INDX), DATA_IS_OK, G_ACADEMIC_LOAD );
                                          PCKG_LOAD_IND := PCKG_LOAD_IND + 1;
                                          IF (L_FLD(INDX) IS NOT NULL) THEN
                                             IF DATA_IS_OK THEN
                                                PCKG_LOAD_OK := PCKG_LOAD_OK + 1;
                                             END IF;
                                          ELSE
                                             PCKG_LOAD_NULLS := PCKG_LOAD_NULLS + 1;
                                          END IF;
                                       ELSE
                                          PCKG_LOAD_NULLS := PCKG_LOAD_NULLS + 1;
                                       END IF;

                                       G_RRRAREQ_TREQ_CODE := 'x';
                                       G_RRRAREQ_TRST_CODE := 'x';
                                       INDX := 9;
                                       IF (FIELD_COUNT >= INDX) THEN
                                          G_RRRAREQ_TREQ_CODE := Nvl(L_FLD(INDX), 'x');
                                          INDX := INDX + 1;
                                          IF ((G_RRRAREQ_TREQ_CODE != 'x') AND (FIELD_COUNT >= INDX)) THEN
                                             G_RRRAREQ_TRST_CODE := Nvl(L_FLD(INDX), 'x');
                                             INDX := INDX + 1;
                                             IF ((G_RRRAREQ_TRST_CODE != 'x') AND (FIELD_COUNT >= INDX)) THEN
                                                G_RRRAREQ_PERIOD := L_FLD(INDX);
                                             END IF;
                                          END IF;
                                          G_AWARD_AIDY  := L_FLD(1);
                                          G_UNM_BANID   := L_FLD(2);
                                          G_FUND_CODE   := L_FLD(3);
                                          G_BANNER_PIDM := GB_COMMON.F_GET_PIDM( G_UNM_BANID );
                                          ANALYSIS_CODE := 0;
                                          IF ((G_RRRAREQ_TREQ_CODE != 'x') AND (G_RRRAREQ_TRST_CODE != 'x')) THEN
                                             ANALYSIS_CODE := UNM.RZKFLIB.F_RRRAREQ_DATA_OK( ANALYSIS_NOTE, G_BANNER_PIDM, G_AWARD_AIDY, G_FUND_CODE, G_RRRAREQ_TREQ_CODE, G_RRRAREQ_TRST_CODE, G_RRRAREQ_PERIOD );
                                             IF (ANALYSIS_CODE = 0) THEN
                                                RRRAREQ_LOAD_NOGO := RRRAREQ_LOAD_NOGO + 1;
                                             ELSE
                                                RRRAREQ_LOAD_OK := RRRAREQ_LOAD_OK + 1;
                                             END IF;
                                          ELSE
                                             G_RRRAREQ_TREQ_CODE := NULL;
                                             G_RRRAREQ_TRST_CODE := NULL;
                                             G_RRRAREQ_PERIOD    := NULL;
                                             RRRAREQ_LOAD_NULLS  := RRRAREQ_LOAD_NULLS + 1;
                                          END IF;
                                       END IF;
                                    END IF;     -- end of: IF (tmpString2 != 'COMMENTONLY') THEN
--==============================================================================
--==============================================================================
-- ==  Improper to try using "FUNDEXCEPTION" and "COMMENTONLY" in same data line
   IF (INSTANCE_NAME = 'DEVL') THEN
      TMPSTRING3 := RPad(L_FLD( 1), Length('FUNDEXCEPTION'))||'  '||RPad(L_FLD( 2),  9)||
              '  '||RPad(L_FLD( 3), Length('COMMENTONLY'))  ||'  '||RPad(Nvl(L_FLD( 4), ' '), 9)||
              '  '||LPad(L_FLD( 5), 10)||'  '||RPad(L_FLD( 6),  5);
      IF (FIELD_COUNT >=  7) THEN TMPSTRING3 := TMPSTRING3||'  '||RPad(L_FLD( 7),  2);END IF;
      IF (FIELD_COUNT >=  8) THEN TMPSTRING3 := TMPSTRING3||'  '||RPad(L_FLD( 8),  2);END IF;
      IF (FIELD_COUNT >=  9) THEN TMPSTRING3 := TMPSTRING3||'  '||RPad(L_FLD( 9),  6);END IF;
      IF (FIELD_COUNT >= 10) THEN TMPSTRING3 := TMPSTRING3||'  '||RPad(L_FLD(10),  2);END IF;
      IF (FIELD_COUNT >= 11) THEN TMPSTRING3 := TMPSTRING3||'  '||RPad(L_FLD(11),  6);END IF;
      Dbms_Output.Put_Line( 'Data ['||To_Char(FILE_READ_COUNT, '90')||'] ==>>'||Trim(TMPSTRING3)||'<<' );
      G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'Data ['||To_Char(FILE_READ_COUNT, '90')||'] ==>>'||Trim(TMPSTRING3)||'<<';
   END IF;
--==============================================================================
--==============================================================================
                                 END DATA_ROW_AWARD_BLOCK;

                                 INDX := 12;
                                 IF (FIELD_COUNT >= INDX) THEN
                                    IF ((Nvl(L_FLD(INDX), 'x')||Nvl(L_FLD(INDX+1), 'x')) != 'xx') THEN
                                       RHACOMM_DATA := RHACOMM_DATA + 1;
                                       IF (TMPSTRING2 = 'COMMENTONLY') THEN
                                          RHACOMM_NO_AWARD := RHACOMM_NO_AWARD + 1;
                                       ELSE
                                          RHACOMM_WITH_AWARD := RHACOMM_WITH_AWARD + 1;
                                       END IF;
                                    END IF;
                                 END IF;
                                 INDX := 14;
                                 IF (FIELD_COUNT >= INDX) THEN
                                    IF ((Nvl(L_FLD(INDX), 'y')||Nvl(L_FLD(INDX+1), 'y')||Nvl(L_FLD(INDX+2), 'y')) != 'yyy') THEN
                                       RORMESG_DATA := RORMESG_DATA + 1;
                                       IF (TMPSTRING2 = 'COMMENTONLY') THEN
                                          RORMESG_NO_AWARD := RORMESG_NO_AWARD + 1;
                                       ELSE
                                          RORMESG_WITH_AWARD := RORMESG_WITH_AWARD + 1;
                                       END IF;
                                    END IF;
                                 END IF;
                                 INDX := 12;
                                 IF (FIELD_COUNT >= INDX) THEN
                                    IF (Nvl(L_FLD(INDX), 'x') != 'x') THEN
                                       RHACOMM_CAT_CODES := RHACOMM_CAT_CODES + 1;
                                    END IF;
                                 END IF;
                              ELSE
--==============================================================================
--==============================================================================
-- ==  Improper to try using "FUNDEXCEPTION" and "COMMENTONLY" in same data line
   IF (INSTANCE_NAME = 'DEVL') THEN
      TMPSTRING3 := RPad(L_FLD( 1), Length('FUNDEXCEPTION'))||'  '||RPad(Nvl(L_FLD( 2), ' '), 9)||
              '  '||RPad(L_FLD( 3), Length('COMMENTONLY'));
      Dbms_Output.Put_Line( 'AWARD Data ['||To_Char(FILE_READ_COUNT, '90')||'] ==>>'||Trim(TMPSTRING3)||'<<' );
      G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'Data ['||To_Char(FILE_READ_COUNT, '90')||'] ==>>'||Trim(TMPSTRING3)||'<<';
   END IF;
--==============================================================================
--==============================================================================
                              END IF;     -- end of: IF (tmpString != 'FUNDEXCEPTION') THEN
                           END IF;     -- end of: IF (Unallowed_Character_Input( Awarding_Data ) = TRUE) THEN
                        END IF;     -- end of: IF (tmpNumber > 1) THEN
                     END DATA_READ_NOT_BLANK_BLOCK;

                  EXCEPTION
                     WHEN NO_DATA_FOUND THEN
                        EXIT;
                     WHEN OTHERS THEN
                        IF SQLCODE != -29284 THEN
                           Dbms_Output.Put_Line( '['||FILE_READ_COUNT||'] Exception in Data_Row_Count_Block -- ' );
                           UNM.RZKFLIB.TRACK_ERRORS;
                        END IF;
                        FILE_READ_ERRORS := FILE_READ_ERRORS + 1;
                  END DATA_ROW_COUNT_BLOCK;
               END LOOP;
--==============================================================================
--==============================================================================
   IF (INSTANCE_NAME = 'DEVL') THEN
      G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~'||CRLF;
   END IF;
--==============================================================================
--==============================================================================

               TOTAL_FIELD_ERRORS := 0;
               FOR IDX IN 1..MAX_FIELDS LOOP
                  TOTAL_FIELD_ERRORS := TOTAL_FIELD_ERRORS + L_FLD_ERR(IDX);
               END LOOP;

               IF (TOTAL_FIELD_ERRORS > 0) THEN
                  OK_TO_PROCESS_INPUT_DATA := FALSE;
                  OK_TO_CALL_RZKSCLD := FALSE;
                  OK_TO_WRITE_NOTES_ONLY := FALSE;
                  DIGIT_COUNT := Length(To_Char(Trunc(FILE_READ_COUNT)));
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'=======================================================';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'There were '||FILE_READ_COUNT||' INPUT data rows read and analyzed.';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'    '||LPad(To_Char(FUND_ECPTN_CNT),        DIGIT_COUNT)||' SFAO non-Special-Rule Restricted awards';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'    '||LPad(To_Char(TOTAL_PARAM_DATA_ROWS), DIGIT_COUNT)||' total Input Parameter Rows';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'    '||LPad(To_Char(TOTAL_COMMENT_ROWS),    DIGIT_COUNT)||' total Input Parameter Rows for Comments ONLY';
                  NMX := TOTAL_FIELD_ERRORS ;
                  IF (TOTAL_FIELD_ERRORS = 1) THEN
                     G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'Data Issue Identified in 1 row of the input file --';
                     Dbms_Output.Put_Line( 'Data Issue Identified in 1 row of the input file --' );
                  ELSE
                     G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'Data Issues Identified in multiple rows of the input file --';
                     Dbms_Output.Put_Line( 'Data Issues Identified in multiple rows of the input file --' );
                  END IF;
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||   '-------------------------------------------------------';
                  IF ( CHAR_TRKR_CNT > 0 ) THEN
                     IF (CHAR_TRKR_ROW_CNT = 1) THEN
                        G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||' ...Unallowed Character(s) ['||CHAR_TRKR_CNT||'] Found in '||CHAR_TRKR_ROW_CNT||' row: ';
                     ELSE
                        G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||' ...Unallowed Character(s) ['||CHAR_TRKR_CNT||'] Found in '||CHAR_TRKR_ROW_CNT||' rows: ';
                     END IF;
                     IF (CHAR_TRKR_CNT > CHAR_TRKR_MAX_CNT) THEN
                        TMPNUMBER := CHAR_TRKR_MAX_CNT;
                     ELSE
                        TMPNUMBER := CHAR_TRKR_CNT;
                     END IF;
                     FOR N IN 1..TMPNUMBER LOOP
                        IF (CHAR_TRKR(N) > 0) THEN
                           G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||' ............ ['||To_Char(CHAR_TRKR(N), '990')||'] -- '||
                                             UNM.RZKFLIB.ASCII_7_DESCRIPTION(CHAR_TRKR(N));
                        END IF;
                     END LOOP;
                     G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||' ........only these are allowed: 1234567890.ABCDEFGHIJKLMNOPQRSTUVWXYZ';
                     G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'              [except in the text fields for RHACOMM or RORMESG]';
                     G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'-------------------------------------------------------';
                  END IF;
                  FOR G IN 1..MAX_FIELDS LOOP
                     F := L_FLD_MSG_ORDER(G);
                     IF (L_FLD_ERR(F) > 0) THEN
                        G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||L_FLD_DESC(F)||FMT_INT(L_FLD_ERR(F), NMX)||' total row(s), '||L_FLD_UNQERR(F)||' unique value(s).';
                        G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||' ........value(s): '||L_FLD_ERRDATA(F);
                        Dbms_Output.Put_Line( L_FLD_DESC(F)||FMT_INT(L_FLD_ERR(F), NMX)||' total row(s), '||L_FLD_UNQERR(F)||' unique value(s).' );
                        Dbms_Output.Put_Line( ' ........value(s): '||L_FLD_ERRDATA(F) );
                     END IF;
                  END LOOP;
                  Dbms_Output.Put_Line( '=======================================================' );
                  Dbms_Output.Put_Line( '*******  Data Issues in the Input File  *******' );
                  Dbms_Output.Put_Line( 'Your request cannot be processed now; fix the data and'  );
                  Dbms_Output.Put_Line( 'try again later.' );
                  Dbms_Output.Put_Line( '=======================================================' );
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF;
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'-------------------------------------------------------';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'***********  Data Issues in the Input File  ***********';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'Your request cannot be processed now; fix the data and';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'try again later.';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'=======================================================';
               ELSE
                  IF ((PARAM_DATA_ROWS_WITH_AWARD > 0) AND (ROW_COUNT = 0)) THEN
                     OK_TO_CALL_RZKSCLD := TRUE;
                  ELSE
                     OK_TO_CALL_RZKSCLD := FALSE;
                  END IF;
                  IF (TOTAL_COMMENT_ROWS = TOTAL_PARAM_DATA_ROWS) THEN
                     OK_TO_WRITE_NOTES_ONLY := TRUE;
                  ELSE
                     OK_TO_WRITE_NOTES_ONLY := FALSE;
                  END IF;
                  IF (OK_TO_CALL_RZKSCLD OR OK_TO_WRITE_NOTES_ONLY) THEN
                     OK_TO_PROCESS_INPUT_DATA := TRUE;
                  END IF;
                  IF (PARAM_DATA_ROWS_WITH_AWARD > 0) THEN
                     IF (TERM_AWARD_COUNT = FILE_READ_COUNT) THEN
                        PCT_TERM := '100.00';
                        PCT_AIDY := '  0.00';
                     ELSIF (AIDY_AWARD_COUNT = FILE_READ_COUNT) THEN
                        PCT_TERM := '  0.00';
                        PCT_AIDY := '100.00';
                     ELSIF ((TERM_AWARD_COUNT > 0) OR (AIDY_AWARD_COUNT > 0)) THEN
                        PCT_TERM := Trim(To_Char((100 * (TERM_AWARD_COUNT / PARAM_DATA_ROWS_WITH_AWARD)), '990.00'));
                        PCT_AIDY := Trim(To_Char((100 * (AIDY_AWARD_COUNT / PARAM_DATA_ROWS_WITH_AWARD)), '990.00'));
                        SELECT Max( LEN ) INTO DIGIT_COUNT
                        FROM
                           (
                           SELECT Length(To_Char(Trunc(PCT_TERM))) LEN FROM DUAL
                        UNION
                           SELECT Length(To_Char(Trunc(PCT_AIDY))) LEN FROM DUAL
                           );
                        PCT_TERM := LPad( PCT_TERM, DIGIT_COUNT+3 );
                        PCT_AIDY := LPad( PCT_AIDY, DIGIT_COUNT+3 );
                     ELSE
                        PCT_TERM := '0.00';
                        PCT_AIDY := '0.00';
                     END IF;
                  END IF;
                  SELECT Length(To_Char(Trunc(FILE_READ_COUNT))) INTO DIGIT_COUNT FROM DUAL;
                  Dbms_Output.Put_Line( ' ...All '||FILE_READ_COUNT||' INPUT data rows are in the correct format; with [Digit_Count='||DIGIT_COUNT||']');
                  Dbms_Output.Put_Line( '    '||  LPad(To_Char(TOTAL_PARAM_DATA_ROWS),        DIGIT_COUNT)||' total Input Parameter Rows');
                  Dbms_Output.Put_Line( '      '||LPad(To_Char(TOTAL_COMMENT_ROWS),           DIGIT_COUNT)||' total Input Parameter Rows for Comments ONLY');
                  Dbms_Output.Put_Line( '      '||LPad(To_Char(PARAM_DATA_ROWS_WITH_AWARD),   DIGIT_COUNT)||' total Input Parameter Rows for Fund Awarding');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(TERM_AWARD_COUNT), DIGIT_COUNT)||' formatted for TERM awards ['||PCT_TERM||PCNT||'], and with');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(AIDY_AWARD_COUNT), DIGIT_COUNT)||' formatted for AIDY awards ['||PCT_AIDY||PCNT||']'          );
                  Dbms_Output.Put_Line( '      '||LPad(To_Char(PCKG_LOAD_IND     ), DIGIT_COUNT)||' total PCKG Load Indicator(s) in file'   );
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(PCKG_LOAD_OK      ), DIGIT_COUNT)||' total OK values [1..5]');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(PCKG_LOAD_NULLS   ), DIGIT_COUNT)||' total NULL values; NULLs default to 1 (Full-Time)');
                  Dbms_Output.Put_Line( '      '||LPad(To_Char(AWRD_LTR_IND_FLAGS), DIGIT_COUNT)||' total AWRD LTR Indicator(s) in file'    );
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(AWRD_LTR_IND_YES  ), DIGIT_COUNT)||' total "Y" values');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(AWRD_LTR_IND_NO   ), DIGIT_COUNT)||' total "N" values');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(AWRD_LTR_IND_NULLS), DIGIT_COUNT)||' total NULL values; NULLs default to "Y"');
                  Dbms_Output.Put_Line( '      '||LPad(To_Char(RRRAREQ_LOAD_OK+RRRAREQ_LOAD_NOGO), DIGIT_COUNT)||' total RRRAREQ data row(s) in file'    );
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(RRRAREQ_LOAD_OK  ), DIGIT_COUNT)||' total with OK values');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(RRRAREQ_LOAD_NOGO), DIGIT_COUNT)||' total with NoGo values');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(RRRAREQ_LOAD_NULLS), DIGIT_COUNT)||' total NULL values; so no RRRAREQ action');
                  Dbms_Output.Put_Line( '      '||LPad(To_Char(RHACOMM_DATA), DIGIT_COUNT)||' total Input Parameter Rows for RHACOMM');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(RHACOMM_NO_AWARD  ), DIGIT_COUNT)||' for Comments Only');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(RHACOMM_WITH_AWARD), DIGIT_COUNT)||' with Fund Awarding');
                  Dbms_Output.Put_Line( '          '||LPad(To_Char(RHACOMM_CAT_CODES), DIGIT_COUNT)||' total Input Parameter Rows for RHACOMM CAT Code');
                  Dbms_Output.Put_Line( '      '||LPad(To_Char(RORMESG_DATA), DIGIT_COUNT)||' total Input Parameter Rows for RORMESG Comments with a CAT Code');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(RORMESG_NO_AWARD  ), DIGIT_COUNT)||' for Comments Only');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(RORMESG_WITH_AWARD), DIGIT_COUNT)||' with Fund Awarding');

                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||' ...All '||FILE_READ_COUNT||' INPUT data rows are in the correct format; with';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        '||LPad(To_Char(SP_FUND_COUNT   ), DIGIT_COUNT)||' SFAO non-Special-Rule Restricted awards';
                  FOR S IN 1..SP_FUND_COUNT LOOP
                     G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        .......... '||SPECIAL_FUND(S);
                  END LOOP;
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'    '||  LPad(To_Char(TOTAL_PARAM_DATA_ROWS),      DIGIT_COUNT)||' total Input Parameter Rows';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'      '||LPad(To_Char(TOTAL_COMMENT_ROWS),         DIGIT_COUNT)||' total Input Parameter Rows for Comments ONLY';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'      '||LPad(To_Char(PARAM_DATA_ROWS_WITH_AWARD), DIGIT_COUNT)||' total Input Parameter Rows for Fund Awarding';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        '||LPad(To_Char(TERM_AWARD_COUNT), DIGIT_COUNT)||' formatted for TERM awards ['||PCT_TERM||PCNT||']';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        '||LPad(To_Char(AIDY_AWARD_COUNT), DIGIT_COUNT)||' formatted for AIDY awards ['||PCT_AIDY||PCNT||']';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'      '||LPad(To_Char(PCKG_LOAD_IND     ), DIGIT_COUNT)||' total PCKG Load Indicator(s) in file';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        '||LPad(To_Char(PCKG_LOAD_OK      ), DIGIT_COUNT)||' total OK values [1..5]';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        '||LPad(To_Char(PCKG_LOAD_NULLS   ), DIGIT_COUNT)||' total NULL values; NULLs default to 1 (Full-Time)';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'      '||LPad(To_Char(AWRD_LTR_IND_FLAGS), DIGIT_COUNT)||' total AWRD LTR Indicator(s) in file';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        '||LPad(To_Char(AWRD_LTR_IND_YES  ), DIGIT_COUNT)||' total "Y" values';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        '||LPad(To_Char(AWRD_LTR_IND_NO   ), DIGIT_COUNT)||' total "N" values';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        '||LPad(To_Char(AWRD_LTR_IND_NULLS), DIGIT_COUNT)||' total NULL values; NULLs default to "Y"';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'      '||LPad(To_Char(RRRAREQ_LOAD_OK+RRRAREQ_LOAD_NOGO), DIGIT_COUNT)||' total RRRAREQ data row(s) in file';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        '||LPad(To_Char(RRRAREQ_LOAD_OK  ), DIGIT_COUNT)||' total with OK values';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        '||LPad(To_Char(RRRAREQ_LOAD_NOGO), DIGIT_COUNT)||' total with NoGo values';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        '||LPad(To_Char(RRRAREQ_LOAD_NULLS), DIGIT_COUNT)||' total NULL values; so no RRRAREQ action';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'      '||LPad(To_Char(RHACOMM_DATA), DIGIT_COUNT)||' total Input Parameter Rows for RHACOMM';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        '||LPad(To_Char(RHACOMM_NO_AWARD  ), DIGIT_COUNT)||' for Comments Only';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        '||LPad(To_Char(RHACOMM_WITH_AWARD), DIGIT_COUNT)||' with Fund Awarding';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'          '||LPad(To_Char(RHACOMM_CAT_CODES), DIGIT_COUNT)||' total Input Parameter Rows for RHACOMM CAT Code';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'      '||LPad(To_Char(RORMESG_DATA), DIGIT_COUNT)||' total Input Parameter Rows for ROAMESG Comments';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        '||LPad(To_Char(RORMESG_NO_AWARD  ), DIGIT_COUNT)||' for Comments Only';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'        '||LPad(To_Char(RORMESG_WITH_AWARD), DIGIT_COUNT)||' with Fund Awarding';
               END IF;

               Utl_File.FCLOSE( G_INPUT_HANDLE );
               IF ((PARAM_DATA_ROWS_WITH_AWARD > 0) AND (ROW_COUNT != 0)) THEN
                  Dbms_Output.Put_Line( '================================================================' );
                  Dbms_Output.Put_Line( '*******  RZRSCIN is NOT empty  *******' );
                  Dbms_Output.Put_Line( 'Your awarding request cannot be processed now.  Try again later.' );
                  Dbms_Output.Put_Line( '================================================================' );
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'================================================================';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'*******  RZRSCIN is NOT empty  *******';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'Your awarding request cannot be processed now.  Try again later.';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'================================================================';
                  RZRSCIN_IND := 999;
               END IF;
               IF OK_TO_PROCESS_INPUT_DATA THEN
                  G_HISTORY_FILENAME := G_BASE_FILENAME||'_'||To_Char(FILE_READ_COUNT)||'_'||C_RPT_DATE_STR||'_'||ONE_UP_NUMBER||G_FILE_EXTENTION;
                  Dbms_Output.Put_Line( '   File_Directory: '||  G_FILE_DIRECTORY );
                  Dbms_Output.Put_Line( '  Input_File_Name: '||  G_INPUT_FILENAME );
                  IF (ROW_COUNT = 0) THEN Dbms_Output.Put_Line( 'Archive_File_Name: '||G_HISTORY_FILENAME ); END IF;
                  Dbms_Output.Put_Line( '      exists_flag: '||            'TRUE' );
                  Dbms_Output.Put_Line( '~~~  ~~~  ~~~  ~~~  ~~~  ~~~  ~~~  ~~~'  );
                  Dbms_Output.Put_Line( '  Input Data Rows: '||FILE_READ_COUNT    );
                  Dbms_Output.Put_Line( '======================================'||CRLF  );

                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'   File_Directory: '||  G_FILE_DIRECTORY;
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'  Input_File_Name: '||  G_INPUT_FILENAME ;
                  IF (ROW_COUNT = 0) THEN G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'Archive_File_Name: '||G_HISTORY_FILENAME; END IF;
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'      exists_flag: '||            'TRUE';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'=======================================================';
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'  Input Data Rows: '||FILE_READ_COUNT;
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'======================================================='||CRLF;

                  G_INPUT_HANDLE := Utl_File.FOPEN(G_FILE_DIRECTORY,         G_INPUT_FILENAME,   'R');

                  FILE_READ_ERRORS    := 0;
                  FILE_READ_COUNT     := 0;
                  FILE_DATA_LENGTH    := 0;
                  UNUSABLE_DATA_COUNT := 0;

                  LOOP
                     <<DATA_PROCESS_BLOCK>>
                     BEGIN
                        Utl_File.GET_LINE(G_INPUT_HANDLE, DOWNLOAD_DATA);
                        DOWNLOAD_DATA := RTRIM_CTRL( DOWNLOAD_DATA );
/*
                        n := Length(Download_Data);
                        IF (n > 1) THEN
                           IF (ASCII(SubStr(Download_Data, 1, n)) < 32) THEN
                              Download_Data := SubStr(Download_Data, 1, n-1);
                           END IF;
                        END IF;
*/
                        FILE_READ_COUNT  := FILE_READ_COUNT + 1;
                        FILE_DATA_LENGTH := Length(DOWNLOAD_DATA);
                        TMPNUMBER := Nvl(Length(REPLACE(DOWNLOAD_DATA, Chr(9), '')), 0);

                        IF ((FILE_READ_COUNT > SP_FUND_COUNT) AND (TMPNUMBER > 0))THEN
                           <<DATA_LENGTH_CHECK>>
                           BEGIN
                              IF (( FILE_DATA_LENGTH > 0 ) AND ( FILE_DATA_LENGTH < 4000  )) THEN
                                 -- Initialize the data fields...
                                 G_UNM_BANID         := NULL;
                                 G_AWARD_AIDY        := NULL;
                                 G_AWARD_TERM        := NULL;
                                 G_FUND_CODE         := NULL;
                                 G_AWARD_AMOUNT      := NULL;
                                 G_AWARD_ACTION      := NULL;
                                 G_AWRD_LTR_IND      := NULL;
                                 G_ACADEMIC_LOAD     := NULL;
                                 G_RHRCOMM_CAT_CODE  := NULL;
                                 G_TEXT_FOR_RHRCOMM  := NULL;
                                 G_EXDY_FOR_RORMESG  := NULL;
                                 G_CODE_FOR_RORMESG  := NULL;
                                 G_TEXT_FOR_RORMESG  := NULL;
                                 G_RRRAREQ_TREQ_CODE := NULL;
                                 G_RRRAREQ_TRST_CODE := NULL;
                                 G_RRRAREQ_PERIOD    := NULL;
                                 --====================================================================
                                 -- Parse the data string into individual fields...
                                 L_FLD := UNM.ARRAYTABLES.F_PARSE_STRING( DOWNLOAD_DATA, HT, FIELD_COUNT );
-- -- -- FOR h IN 1..Field_Count LOOP
-- -- --    Dbms_Output.Put_Line( '~~~~~['||Field_Count||']~~~~~ L_fld('||To_Char(h, '90')||') ==>'||SubStr(L_fld(h), 1, 35)||'<==' );
-- -- -- END LOOP;
                                 G_AWARD_AIDY   :=           L_FLD(1);
                                 G_UNM_BANID    :=           L_FLD(2);
                                 G_FUND_CODE    :=           L_FLD(3);
                                 G_AWARD_TERM   :=           L_FLD(4);
                                 G_AWARD_AMOUNT := To_Number(L_FLD(5));
                                 G_AWARD_ACTION :=    SubStr(L_FLD(6), 1, 4);
                                 /*==================================================================*/
                                 IF (FIELD_COUNT >=  7) THEN G_AWRD_LTR_IND      := Upper(L_FLD(7)); END IF;
                                 IF (FIELD_COUNT >=  8) THEN G_ACADEMIC_LOAD     := L_FLD( 8);       END IF;
                                 IF (FIELD_COUNT >=  9) THEN G_RRRAREQ_TREQ_CODE := L_FLD( 9);       END IF;
                                 IF (FIELD_COUNT >= 10) THEN G_RRRAREQ_TRST_CODE := L_FLD(10);       END IF;
                                 IF (FIELD_COUNT >= 11) THEN G_RRRAREQ_PERIOD    := L_FLD(11);       END IF;
                                 IF (FIELD_COUNT >= 12) THEN G_RHRCOMM_CAT_CODE  := L_FLD(12);       END IF;
                                 IF (FIELD_COUNT >= 13) THEN G_TEXT_FOR_RHRCOMM  := L_FLD(13);       END IF;
                                 IF (FIELD_COUNT >= 14) THEN PROC_RORMESG_DATE_CK(  L_FLD(14), DATA_IS_OK, G_EXDY_FOR_RORMESG ); END IF;
                                 IF (FIELD_COUNT >= 15) THEN G_CODE_FOR_RORMESG  := L_FLD(15);       END IF;
                                 IF (FIELD_COUNT >= 16) THEN G_TEXT_FOR_RORMESG  := L_FLD(16);       END IF;

                                 TMPSTRING2 := Upper(Nvl(L_FLD(3), 'N/A'));
                                 IF (TMPSTRING2 = 'COMMENTONLY') THEN
                                    G_BANNER_PIDM   := F_GET_PIDM( G_UNM_BANID );
                                    SOME_DATA := FALSE;
                                    INDX := 12;
                                    IF (FIELD_COUNT >= INDX) THEN
                                       IF ((Nvl(L_FLD(INDX), 'x')||Nvl(L_FLD(INDX+1), 'x')) != 'xx') THEN
                                          SOME_DATA := TRUE;
                                          LOAD_CHECK := UNM.RZKFLIB.F_INSERT_RHRCOMM_RECORD( G_BANNER_PIDM, G_AWARD_AIDY, G_TEXT_FOR_RHRCOMM, G_RHRCOMM_CAT_CODE );
                                          IF LOAD_CHECK THEN RHRCOMM_LOAD_OK := RHRCOMM_LOAD_OK + 1;
                                          ELSE RHRCOMM_LOAD_FAILED := RHRCOMM_LOAD_FAILED + 1; END IF;
                                       END IF;
                                    END IF;
                                    INDX := 14;
                                    IF (FIELD_COUNT >= INDX) THEN
                                       IF ((Nvl(L_FLD(INDX), 'y')||Nvl(L_FLD(INDX+1), 'y')||Nvl(L_FLD(INDX+2), 'y')) != 'yyy') THEN
                                          SOME_DATA := TRUE;
                                          LOAD_CHECK := UNM.RZKFLIB.F_INSERT_RORMESG_RECORD( G_BANNER_PIDM, G_AWARD_AIDY, G_EXDY_FOR_RORMESG, G_CODE_FOR_RORMESG, G_TEXT_FOR_RORMESG );
                                          IF LOAD_CHECK THEN RORMESG_LOAD_OK := RORMESG_LOAD_OK + 1;
                                          ELSE RORMESG_LOAD_FAILED := RORMESG_LOAD_FAILED + 1; END IF;
                                       END IF;
                                    END IF;
                                    IF SOME_DATA THEN
                                       SOME_COMMENT_ONLY_DATA := SOME_COMMENT_ONLY_DATA + 1;
                                    END IF;
                                 ELSE
                                    BEGIN
                                       INSERT INTO RZRSCIN(RZRSCIN_UNM_ID,
                                                           RZRSCIN_AIDY_CODE,
                                                           RZRSCIN_TERM_CODE,
                                                           RZRSCIN_FUND_CODE,
                                                           RZRSCIN_AWARD_AMT,
                                                           RZRSCIN_AWST_CODE,
                                                           RZRSCIN_AWRD_LTR_IND,
                                                           RZRSCIN_PCKG_LOAD_IND,
                                                           RZRSCIN_RRRAREQ_TREQ,
                                                           RZRSCIN_RRRAREQ_TRST,
                                                           RZRSCIN_RRRAREQ_PRD,
                                                           RZRSCIN_RHRCOMM_CAT,
                                                           RZRSCIN_RHRCOMM_TXT,
                                                           RZRSCIN_RORMESG_EXPR,
                                                           RZRSCIN_RORMESG_CODE,
                                                           RZRSCIN_RORMESG_TXT
                                                          )
                                       VALUES(G_UNM_BANID   ,
                                              G_AWARD_AIDY  ,
                                              G_AWARD_TERM  ,
                                              G_FUND_CODE   ,
                                              G_AWARD_AMOUNT,
                                              G_AWARD_ACTION,
                                              G_AWRD_LTR_IND,
                                              G_ACADEMIC_LOAD,
                                              G_RRRAREQ_TREQ_CODE,
                                              G_RRRAREQ_TRST_CODE,
                                              G_RRRAREQ_PERIOD,
                                              G_RHRCOMM_CAT_CODE,
                                              G_TEXT_FOR_RHRCOMM,
                                              G_EXDY_FOR_RORMESG,
                                              G_CODE_FOR_RORMESG,
                                              G_TEXT_FOR_RORMESG
                                             );

                                       COMMIT;
                                       --ROLLBACK;
                                       TABLE_INSERT_COUNT := TABLE_INSERT_COUNT + 1;
                                    EXCEPTION
                                       WHEN OTHERS THEN
                                          Dbms_Output.Put_Line( '['||FILE_READ_COUNT||'] Exception INSERTING Data, File_Data_Length== '||FILE_DATA_LENGTH );
                                          UNM.RZKFLIB.TRACK_ERRORS;
                                    END;
                                 END IF;
                              ELSE
                                 Dbms_Output.Put_Line( '['||FILE_READ_COUNT||'] File Data is not usable, the Data read is '||FILE_DATA_LENGTH||' characters long' );
                                 UNUSABLE_DATA_COUNT := UNUSABLE_DATA_COUNT + 0;
                              END IF;  -- end of: IF (( File_Data_Length > 0 ) ...) THEN

                           EXCEPTION
                              WHEN OTHERS THEN
                                 Dbms_Output.Put_Line( '['||FILE_READ_COUNT||'] Exception in Data_Length_Check -- ' );
                                 UNM.RZKFLIB.TRACK_ERRORS;
                           END DATA_LENGTH_CHECK;
                        END IF;

                     EXCEPTION
                        WHEN NO_DATA_FOUND THEN
                           EXIT;
                        WHEN OTHERS THEN
                           IF SQLCODE != -29284 THEN
                              Dbms_Output.Put_Line( '['||FILE_READ_COUNT||'] Exception in Data_Process_Block -- ' );
                              UNM.RZKFLIB.TRACK_ERRORS;
                           END IF;
                           FILE_READ_ERRORS := FILE_READ_ERRORS + 1;
                     END DATA_PROCESS_BLOCK;
                  END LOOP;
IF ((INSTANCE_NAME = 'DEVL') AND (USER_NAME != 'DASMITH')) THEN
                  Utl_File.FCOPY( G_FILE_DIRECTORY, G_INPUT_FILENAME, G_FILE_HISTORY_DIRECTORY, G_HISTORY_FILENAME );
                  Utl_File.FCLOSE( G_INPUT_HANDLE   );
                  Utl_File.FREMOVE( G_FILE_DIRECTORY, G_INPUT_FILENAME );
END IF;
               ELSE
                  Utl_File.FCLOSE( G_INPUT_HANDLE   );
               END IF;   -- end of: IF ((Total_Field_ERRORS = 0) AND (ROW_Count = 0)) THEN

               Utl_File.FCLOSE( G_INPUT_HANDLE   );

            END IF;  -- end of: IF Data_Import_File_Exists THEN

            IF (TABLE_INSERT_COUNT = 0) THEN
               RZRSCIN_IND := 999;
            ELSE
               RZRSCIN_IND := 0;
            END IF;

            IF DATA_IMPORT_FILE_EXISTS THEN
--                RZRSCIN_ind := 0;
               Dbms_Output.Put_Line( 'Data_Import_File_Exists: TRUE  ==>  RZRSCIN_ind: '||RZRSCIN_IND );
            ELSE
--                RZRSCIN_ind := 999;
               Dbms_Output.Put_Line( 'Data_Import_File_Exists: FALSE  ==>  RZRSCIN_ind: '||RZRSCIN_IND );
            END IF;

            SELECT Count(*) INTO CNT FROM RZRSCIN;

            --=============================================================================
            -- Send an email to the user with the summary data...
            --  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~
            IF (SOME_COMMENT_ONLY_DATA > 0) THEN
               IF (TOTAL_COMMENT_ROWS = 1) THEN
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||CRLF||'There was 1 row of data for COMMENTONLY'||CRLF;
               ELSE
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||CRLF||'There were '||TOTAL_COMMENT_ROWS||' rows of data for COMMENTONLY'||CRLF;
               END IF;
               XX := TOTAL_COMMENT_ROWS - SOME_COMMENT_ONLY_DATA;
               IF ( XX = 1 ) THEN
                  G_MESSAGE_BODY := G_MESSAGE_BODY||'     1 row for COMMENTONLY with NO DATA'||CRLF;
               ELSIF (XX > 1) THEN
                  G_MESSAGE_BODY := G_MESSAGE_BODY||'     '||XX||' rows for COMMENTONLY with NO DATA'||CRLF;
               END IF;
               XX := RHRCOMM_LOAD_OK + RHRCOMM_LOAD_FAILED;
               IF (XX = 1) THEN
                  G_MESSAGE_BODY := G_MESSAGE_BODY||'     1 row of data for RHACOMM: ';
               ELSE
                  G_MESSAGE_BODY := G_MESSAGE_BODY||'     '||XX||' rows of data for RHACOMM: ';
               END IF;
               G_MESSAGE_BODY := G_MESSAGE_BODY||RHRCOMM_LOAD_OK||' loaded OK and '||RHRCOMM_LOAD_FAILED||' FAILED'||CRLF;
               XX := RORMESG_LOAD_OK + RORMESG_LOAD_FAILED;
               IF (XX = 1) THEN
                  G_MESSAGE_BODY := G_MESSAGE_BODY||'     1 row of data for ROAMESG: ';
               ELSE
                  G_MESSAGE_BODY := G_MESSAGE_BODY||'     '||XX||' rows of data for ROAMESG: ';
               END IF;
               G_MESSAGE_BODY := G_MESSAGE_BODY||RORMESG_LOAD_OK||' loaded OK and '||RORMESG_LOAD_FAILED||' FAILED'||CRLF;
            END IF;

            IF (RZRSCIN_IND > 0) THEN
               G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'No data was loaded into RZRSCIN'||CRLF;
            ELSE
               G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||TABLE_INSERT_COUNT||' rows of data were loaded into RZRSCIN'||CRLF;
            END IF;

            IF (CNT = 1) THEN
               IF (CNT = ROW_COUNT) THEN
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'There are the same number of rows of data [' || CNT || '] in RZRSCIN'||CRLF;
               ELSE
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'There is now 1 row of data in RZRSCIN'||CRLF;
               END IF;
            ELSE
               IF (CNT = ROW_COUNT) THEN
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'There are the same number of rows of data [' || CNT || '] in RZRSCIN'||CRLF;
               ELSE
                  G_MESSAGE_BODY := G_MESSAGE_BODY||CRLF||'There are now ' || CNT || ' rows of data in RZRSCIN'||CRLF;
               END IF;
            END IF;
         END IF;  -- end of: IF (User_Name != 'FINAIDAPPWORX') THEN
      END READ_DATA_BLOCK;

      RZKFLIB.P_USER_EMAIL( 'RZFLSCP_'||INSTANCE_NAME, 'Load Scholarship Parameters [dB: '||INSTANCE_NAME||']',
                            G_MESSAGE_BODY, 'stirlingcrow@unm.edu' );
      
      --StirlingCrow - 2014-02-26: Stopped sending stuff to the FinAid Tech ListServ
      --rzkflib.p_user_email( 'RZFLSCP_'||Instance_Name, 'Load Scholarship Parameters [dB: '||Instance_Name||']',
      --                      G_Message_Body, 'FINAID-TECH-L@unm.edu' );                      
                            
   END USER_NAME_OK_BLOCK;
   --=============================================================================

   Dbms_Output.new_line;

   --========================================================================
   -- Search the Dbms_Output buffer for the text "ERROR" and replace all
   -- instances with the text "ISSUE"
   --------------------------------------------------------------------------
   P_DBMS_TEXT_REPLACE( 'ERROR', 'ISSUE' );
   P_DBMS_TEXT_REPLACE( 'error', 'ISSUE' );
   P_DBMS_TEXT_REPLACE( 'ORA-',  'Oracle-' );

   --========================================================================
   -- This function returns the value of "Function_Status" which is equal to
   -- the sum of all data issues detected, including an indicator value for
   -- loading the data into the SCholarship INput table, RZRSCIN.
   --------------------------------------------------------------------------
   FUNCTION_STATUS := UNUSABLE_DATA_COUNT + FILE_READ_ERRORS + TOTAL_FIELD_ERRORS + RZRSCIN_IND + UNATHORIZED_USER;
   IF (USER_NAME != 'FINAIDAPPWORX') THEN
      Dbms_Output.Put_Line( 'Function_Status: '||FUNCTION_STATUS||
            ' == Unusable_Data_Count + File_Read_ERRORS + Total_Field_ERRORS + RZRSCIN_ind + Unathorized_User' );
      Dbms_Output.Put_Line( '                   == '||UNUSABLE_DATA_COUNT||' + '||
            FILE_READ_ERRORS||' + '||TOTAL_FIELD_ERRORS||' + '||RZRSCIN_IND||' + '||UNATHORIZED_USER );
   END IF;

--    Function_Status := 99;
--    Dbms_Output.Put_Line( '...reset for testing ==> Function_Status: '||Function_Status );

   RETURN FUNCTION_STATUS;

   EXCEPTION
      WHEN OTHERS THEN
         UNM.RZKFLIB.TRACK_ERRORS;
      RETURN -5;

END RZFLSCP;
/

CREATE OR REPLACE PUBLIC SYNONYM RZFLSCP FOR UNM.RZFLSCP;
