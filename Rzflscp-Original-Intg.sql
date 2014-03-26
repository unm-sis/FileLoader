DROP FUNCTION UNM.RZFLSCP;

CREATE OR REPLACE FUNCTION UNM.RZFLSCP( Banner_One_Up_Num IN NUMBER DEFAULT NULL )
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

   G_Number                      NUMBER;
   Function_Status               NUMBER;

   C_rpt_date_str                CONSTANT VARCHAR2(20 CHAR)  DEFAULT TO_CHAR(SYSDATE,'YYYYMMDDHH24MI');
   One_Up_Number                 NUMBER := NULL;
   G_INPUT_handle                Utl_File.file_type;
   G_HISTORY_handle              Utl_File.file_type;
   G_BASE_filename               VARCHAR2(  64 CHAR) DEFAULT NULL;
   G_INPUT_filename              VARCHAR2(  64 CHAR) DEFAULT NULL;
   G_HISTORY_filename            VARCHAR2(  64 CHAR) DEFAULT NULL;
   G_file_directory              VARCHAR2(  64 CHAR) := 'FINAID_SCHOLARSHIP_LOAD';
   G_file_history_directory      VARCHAR2(  64 CHAR) := 'FINAID_SCH_LOAD_HISTORY';
   G_file_extention              VARCHAR2(  20 CHAR) := '.txt';
   HT                            VARCHAR2(  10 CHAR) := CHR(9);
   CrLf                          VARCHAR2(   5 CHAR) := Chr(13) || Chr(10);
   Pcnt                          VARCHAR2(   5 CHAR) := Chr(37);
   User_Name                     VARCHAR2(  50 CHAR);
   MailTo_Address                VARCHAR2( 100 CHAR);
   Instance_Name                 VARCHAR2(  50 CHAR);
   -- String length constraints must be in range (1 .. 32767)
   G_Message_Body                VARCHAR2(9000 CHAR);    --7500 CHAR);
   TERMS_NOT_OK                  BOOLEAN;

   Nmx                           NUMBER := 25;
   cnt                           NUMBER;
   xx                            NUMBER;
   Digit_Count                   NUMBER;
   Current_AIDY                  VARCHAR2( 10 CHAR);
   Current_TERM                  VARCHAR2( 10 CHAR);

   Data_Import_File_Exists       BOOLEAN := FALSE;
   exists_flag                   BOOLEAN;
   file_length                   NUMBER;
   blocksize                     NUMBER;

   Internal_Version              NUMBER;

   G_UNM_BanID                   VARCHAR2( 100 CHAR);
   G_AWARD_AIDY                  VARCHAR2( 100 CHAR);
   G_FUND_Code                   VARCHAR2( 100 CHAR);
   G_AWARD_Action                VARCHAR2( 100 CHAR) := 'ACCP';
   G_AWARD_TERM                  VARCHAR2( 100 CHAR);
   G_AWARD_Amount                NUMBER;
   G_RRRAREQ_TREQ_CODE           VARCHAR2(  10 CHAR);
   G_RRRAREQ_TRST_CODE           VARCHAR2(   4 CHAR);
   G_RRRAREQ_PERIOD              VARCHAR2(  15 CHAR);
   RRRAREQ_Load_OK               NUMBER := 0;
   RRRAREQ_Load_NoGo             NUMBER := 0;
   RRRAREQ_Load_NULLs            NUMBER := 0;
   Analysis_Note                 VARCHAR2( 100 CHAR);
   Analysis_Code                 NUMBER;


   Some_Data                     BOOLEAN;
   Some_Comment_ONLY_Data        NUMBER := 0;
   LOAD_Check                    BOOLEAN;
   RHRCOMM_Load_OK               NUMBER := 0;
   RHRCOMM_Load_FAILED           NUMBER := 0;
   RORMESG_Load_OK               NUMBER := 0;
   RORMESG_Load_FAILED           NUMBER := 0;

   G_Text_for_RHRCOMM            VARCHAR2(4000 CHAR);
   G_RHRCOMM_Cat_Code            VARCHAR2(  10 CHAR);
   G_EXDY_for_RORMESG            NUMBER;
   G_CODE_for_RORMESG            VARCHAR2(  10 CHAR);
   G_Text_for_RORMESG            VARCHAR2(2000 CHAR);
   G_Banner_PIDM                 NUMBER;
   G_RORMESG_Date_Period         NUMBER;
   G_Academic_Load               VARCHAR2(   5 CHAR);
   G_AWRD_LTR_IND                VARCHAR2(   5 CHAR);
   G_Next_FASP_TERM              VARCHAR2(  10 CHAR);

   pct_TERM                      VARCHAR2(  10 CHAR);
   pct_AIDY                      VARCHAR2(  10 CHAR);
   AIDY_Award_Count              NUMBER := 0;
   TERM_Award_Count              NUMBER := 0;

   RZRSCIN_ind                   NUMBER := 0;
   Unathorized_User              NUMBER := 0;
   Total_Field_ERRORS            NUMBER := 0;
   File_Read_ERRORS              NUMBER := 0;
   File_Read_Count               NUMBER := 0;
   File_Data_Length              NUMBER := 0;
   Table_Insert_Count            NUMBER := 0;
   Unusable_Data_Count           NUMBER := 0;
   Download_Data                 VARCHAR2(25000 CHAR);
   Awarding_Data                 VARCHAR2(  500 CHAR);
   --=======================================================
   ERR_NOTE_CHARS_MAX            CONSTANT NUMBER := 50;
   --=======================================================
   --=======================================================
   Bad_Chars                     VARCHAR2(500 CHAR) := '';
   Bad_Chars2                    VARCHAR2(500 CHAR) := '';
   exists_flag2                  BOOLEAN;
   f                             NUMBER;
   n                             NUMBER;
   indx                          NUMBER;

   L_TERM                        VARCHAR2(  6 CHAR);
   L_AIDY                        VARCHAR2(  6 CHAR);
   L_FUND                        VARCHAR2(  6 CHAR);

--   TYPE tString IS TABLE OF VARCHAR2(5000) INDEX BY BINARY_INTEGER;
   L_fld                         unm.ArrayTables.tString;
   L_fld_desc                    unm.ArrayTables.tString;
   L_fld_errdata                 unm.ArrayTables.tString;
   L_fld_err                     unm.ArrayTables.tNumber;
   L_fld_unqerr                  unm.ArrayTables.tNumber;
   L_fld_MSG_Order               unm.ArrayTables.tNumber;
   DATA_is_OK                    BOOLEAN := FALSE;
   MAX_Fields                    NUMBER  := 19;

   L_ERROR_FIELD                 NUMBER  := 0;

   Field_Count                   NUMBER;
   char_trkr                     unm.ArrayTables.tNumber;
   char_trkr_cnt                 NUMBER := 0;
   char_trkr_max_cnt             NUMBER := 100;
   char_trkr_row_cnt             NUMBER := 0;
   ROW_Count                     NUMBER;
   Special_Fund                  unm.ArrayTables.tString;
   Sp_Fund_Count                 NUMBER := 0;
   Fund_Ecptn_Cnt                NUMBER := 0;
   Total_Param_Data_ROWS         NUMBER := 0;
   Param_Data_ROWS_with_Award    NUMBER := 0;
   PCKG_LOAD_IND                 NUMBER := 0;
   PCKG_LOAD_OK                  NUMBER := 0;
   PCKG_LOAD_NULLs               NUMBER := 0;
   AWRD_LTR_IND_Flags            NUMBER := 0;
   AWRD_LTR_IND_NULLs            NUMBER := 0;
   AWRD_LTR_IND_Yes              NUMBER := 0;
   AWRD_LTR_IND_No               NUMBER := 0;
   Total_Comment_ROWS            NUMBER := 0;
   Comment_ROWS_with_Award       NUMBER := 0;
   Comment_ROWS_No_Award         NUMBER := 0;
   RHACOMM_Cat_Codes             NUMBER := 0;
   RHACOMM_Data                  NUMBER := 0;
   RORMESG_Data                  NUMBER := 0;
   RHACOMM_No_Award              NUMBER := 0;
   RHACOMM_with_Award            NUMBER := 0;
   RORMESG_No_Award              NUMBER := 0;
   RORMESG_with_Award            NUMBER := 0;
   tmpNumber                     NUMBER;
   tmpString                     VARCHAR2( 500 CHAR);
   tmpString2                    VARCHAR2( 500 CHAR);
   tmpString3                    VARCHAR2( 500 CHAR);
   OK_to_Process_Input_Data      BOOLEAN := FALSE;
   OK_to_Call_RZKSCLD            BOOLEAN := FALSE;
   OK_to_Write_Notes_Only        BOOLEAN := FALSE;

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
   FUNCTION f_api_version
         RETURN NUMBER IS
      BEGIN
         RETURN (CURRENT_RELEASE);
   END f_api_version;

   FUNCTION Unallowed_Character_Input( String IN VARCHAR2 )
         RETURN BOOLEAN
   IS
      j                       NUMBER;
      k                       NUMBER;
      String_Length           NUMBER;
      String_2                VARCHAR2(500 CHAR);
      String_3                VARCHAR2(500 CHAR);
      NOT_Found               BOOLEAN := TRUE;
      Char_ERR                BOOLEAN := FALSE;
   BEGIN
      k := char_trkr_cnt;
      String_Length := Nvl(Length( String ), 0);
      IF (String_Length > 0) THEN
         FOR i IN 1..String_Length LOOP
            j := Ascii(SubStr(String, i, 1));
            IF ((j != 46) AND (j NOT BETWEEN 0 AND 31) AND (j NOT BETWEEN 48 AND 57) AND (j NOT BETWEEN 65 AND 90)) THEN
               Char_ERR := TRUE;
               IF (char_trkr_cnt = 0) THEN
                  char_trkr_cnt := char_trkr_cnt + 1;
                  char_trkr(char_trkr_cnt) := j;
               ELSE
                  FOR c IN 1..char_trkr_cnt LOOP
                     IF (char_trkr(c) = j) THEN
                        NOT_Found := FALSE;
                     END IF;
                  END LOOP;
                  IF NOT_Found THEN
                     char_trkr_cnt := char_trkr_cnt + 1;
                     char_trkr(char_trkr_cnt) := j;
                  END IF;
               END IF;
            END IF;
         END LOOP;
      END IF;
      IF Char_ERR THEN
         char_trkr_row_cnt := char_trkr_row_cnt + 1;
-- -- -- Dbms_Output.Put_Line( '~~~~~~~~~~ Unallowed_Character_Input [char_trkr_row_cnt: '||char_trkr_row_cnt||', char_trkr_cnt: '||char_trkr_cnt||'] String ==>'||String||'<==  has problem(s)' );
         RETURN TRUE;
      ELSE
         RETURN FALSE;
      END IF;
   END Unallowed_Character_Input;

   FUNCTION Append_Text(String IN OUT VARCHAR2, Text IN VARCHAR2,
                        Dlmtr  IN VARCHAR2,     MAX_CHAR_for_String IN NUMBER)
         RETURN BOOLEAN
   IS
      Text_Length             NUMBER;
      String_Length           NUMBER;
      Delimiter_Length        NUMBER;
      New_String_Length       NUMBER;
      Delimiter               VARCHAR2(5 CHAR);
      Unique_Text             BOOLEAN := TRUE;

   BEGIN
      String_Length := Length( Nvl(String, ' ' ));
      IF ( Nvl(String, ' ' ) = ' ' ) THEN
         String := Text;
      ELSE
         Delimiter         := SubStr( Nvl( Dlmtr, '--' ), 1, 5 );
         Delimiter_Length  := Length( Delimiter );
         Text_Length       := Length( Text );
         New_String_Length := String_Length + Delimiter_Length + Text_Length;
         IF ( InStr( String, Text ) = 0 ) THEN
            IF ( New_String_Length < MAX_CHAR_for_String ) THEN
               String := String || Delimiter || Text;
            END IF;
         ELSE
            Unique_Text := FALSE;
         END IF;
      END IF;

      RETURN Unique_Text;
   END Append_Text;

   PROCEDURE Append_Error_Note( String IN OUT VARCHAR2, Text IN VARCHAR2, numb IN OUT NUMBER )
   IS
      MAX_LEN           CONSTANT NUMBER := 50;
   BEGIN
      IF (Append_Text(String, Text, '~', MAX_LEN)) THEN
         numb := numb + 1;
      END IF;
   END Append_Error_Note;


      /*========================================================================================
         RZBSCAM -- Financial Aid Scholarship Fund Award Amounts; including the Default Award
                        Amount for the fund in the aid year.
         RZBSCFR -- The Financial Aid Fund Rules - Used to determine which funds are mutually
                        exclusive and which funds need adjustment with other funds.
         RZBSCLU -- The Scholarship Award/Denial List lookup table. Includes:
                        The limit of PE courses per term.
                        The maximum number of terms a person is allowed have for this fund code.
                        The column number of the 0506 ROBUSDF table with fund terms used.
                        The first term, year,       quit gpa and hours required.
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

   FUNCTION DATA_Issue_33(idx IN NUMBER, RAW_Data IN VARCHAR2) RETURN BOOLEAN
   IS
   BEGIN
-- -- -- Dbms_Output.Put_Line( '~~~~~~~~~~ DATA_Issue_33(idx ==>'||idx||'<==  RAW_Data: '||Nvl(RAW_Data, 'null')||' )' );
-- Dbms_Output.Put_Line( '~~~~~~~~~~ DATA_Issue_33(idx ==>'||idx||'<==  RAW_Data: '||Nvl(RAW_Data, 'null')||' )' );

      L_fld_err(idx) := L_fld_err(idx) + 1;
      Append_Error_Note(L_fld_errdata(idx), RAW_Data, L_fld_unqerr(idx));

-- Dbms_Output.Put_Line( '~~~~~~~~~~ DATA_Issue_33(idx ==>'||idx||'<==  RAW_Data: '||Nvl(RAW_Data, 'null')||' )  ==>'||L_fld_err(idx)||'<==>'||L_fld_unqerr(idx) );
      RETURN FALSE;
   END DATA_Issue_33;

   PROCEDURE Proc_AWRD_LTR_CK( p7 IN OUT VARCHAR2, DATA_Looks_Good IN OUT BOOLEAN )
   IS
      str1              VARCHAR2( 25 CHAR);
   BEGIN
      str1 := Upper(Nvl(p7, 'Y'));
      IF ((str1 != 'Y') AND (str1 != 'N')) THEN
         DATA_Looks_Good := DATA_Issue_33(7, Nvl(p7, 'Y'));
      ELSE
         p7 := Upper(p7);
         DATA_Looks_Good := TRUE;
      END IF;
   END Proc_AWRD_LTR_CK;

   PROCEDURE Proc_ACAD_LD_CK( p8 IN VARCHAR2, DATA_Looks_Good IN OUT BOOLEAN, ACLD OUT VARCHAR2 )
   IS
      load              NUMBER := -1;
      str1              VARCHAR2( 25 CHAR);
   BEGIN
      str1 := Nvl(p8, '1');
      IF (unm.rzkflib.is_numeric(str1) = TRUE) THEN
         load := To_Number(str1);
         IF ((load < 1) OR (load > 5)) THEN
            load := -1;
            DATA_Looks_Good := DATA_Issue_33(8, Nvl(p8, 'NULL'));
         ELSE
            DATA_Looks_Good := TRUE;
         END IF;
      ELSE
         load := -1;
         DATA_Looks_Good := DATA_Issue_33(8, Nvl(p8, 'NULL'));
      END IF;
      ACLD := Trim(To_Char(load));
   END Proc_ACAD_LD_CK;

   PROCEDURE Proc_ACTION_CK( p6 IN VARCHAR2, DATA_Looks_Good IN OUT BOOLEAN )
   IS
      L_ACTION                   VARCHAR2(  6 CHAR);
   BEGIN
      L_ACTION := SubStr(Nvl(p6, 'NULL'), 1, 4);
      IF ((L_ACTION != 'ACCP') AND (L_ACTION != 'CANC')) THEN
         DATA_Looks_Good := DATA_Issue_33(6, L_ACTION);
      ELSE
         DATA_Looks_Good := TRUE;
      END IF;
   END Proc_ACTION_CK;

   FUNCTION Award_Amount_Check(fp_AIDY IN VARCHAR2, fp_FUND IN VARCHAR2, fp_Amount IN NUMBER, fp_Status OUT NUMBER)
         RETURN BOOLEAN
      IS
         Upper_Amount      NUMBER;
         Lower_Amount      NUMBER;
         Allocated_Amount  NUMBER;

      BEGIN
         fp_Status := 10;
         SELECT Max(MAX_AMT), Max(MIN_AMT), Max(ALLOC_AMT)
         INTO Upper_Amount, Lower_Amount, Allocated_Amount
         FROM
            (
            SELECT RFRASPC_MAX_AWARD_AMT  MAX_AMT, RFRASPC_MIN_AWARD_AMT  MIN_AMT, RFRASPC_TOTAL_ALLOC_AMT  ALLOC_AMT
            FROM RFRASPC WHERE RFRASPC_AIDY_CODE = fp_AIDY AND RFRASPC_FUND_CODE = fp_FUND
         UNION
            SELECT 0 MAX_AMT, 0 MIN_AMT, 0 ALLOC_AMT FROM dual
            );

         CASE
            WHEN fp_Amount > Upper_Amount     THEN fp_Status :=  1;
            WHEN fp_Amount < Lower_Amount     THEN fp_Status := -1;
            WHEN fp_Amount > Allocated_Amount THEN fp_Status :=  5;
            ELSE fp_Status := 0;
         END CASE;

--          Dbms_Output.Put_Line( 'Award_Amount_Check('||fp_AIDY||', '||fp_FUND||', '||fp_Amount||', Status ==> '||fp_Status||')' );
         IF (fp_Status = 0) THEN
            RETURN TRUE;
         ELSE
            RETURN FALSE;
         END IF;
      EXCEPTION WHEN OTHERS THEN
         RETURN FALSE;
   END Award_Amount_Check;

   PROCEDURE Proc_AMOUNT_CK( p5 IN VARCHAR2, DATA_Looks_Good IN OUT BOOLEAN )
   IS
      Amount_OK            BOOLEAN := FALSE;
      Amount_Status        NUMBER  := 10;
   BEGIN
      IF (unm.rzkflib.is_numeric(p5) = TRUE) THEN
         Amount_OK := Award_Amount_Check(L_AIDY, L_FUND, p5, Amount_Status );

--          IF (p5 < 0) THEN
--             DATA_Looks_Good := DATA_Issue_33(5, Nvl(p5, 'NULL'));
--                L_fld_desc( 5) := ' AMOUNT non-Numeric.........:';     L_fld_MSG_Order( 5) :=  4;
--                L_fld_desc(19) := ' AMOUNT Exceeds Fund Max-Min:';     L_fld_MSG_Order(19) :=  5;
         IF (Amount_Status != 0) THEN
            DATA_Looks_Good := DATA_Issue_33(19, Nvl(p5, 'NULL')||'['||L_FUND||']');
         ELSE
            DATA_Looks_Good := TRUE;
         END IF;
      ELSE
         DATA_Looks_Good := DATA_Issue_33(5, Nvl(p5, 'NULL'));
      END IF;
   END Proc_AMOUNT_CK;

   PROCEDURE Proc_TERM_CODE_CK( p1 IN VARCHAR2, AIDY_OK IN BOOLEAN, p4 IN VARCHAR2, DATA_Looks_Good IN OUT BOOLEAN )
   IS
   BEGIN
      L_TERM := Nvl(p4, 'NULL');
      IF L_TERM = p4 THEN
         IF ((L_TERM < Current_TERM) OR
             (RZKFLIB.TERM_Not_Valid( L_TERM )) OR
             (unm.rzkflib.is_numeric( p4 ) = FALSE)) THEN
            DATA_Looks_Good := DATA_Issue_33(4, p4);
         ELSE
            L_ERROR_FIELD := 14;
            IF AIDY_OK THEN
               IF (RZKFLIB.AIDY_TERM_Mismatch( L_AIDY, L_TERM )) THEN
                  DATA_Looks_Good := DATA_Issue_33(14, p1||'~'||p4);
               ELSE
                  DATA_Looks_Good := TRUE;
               END IF;
            END IF;
         END IF;
      ELSIF (L_TERM = 'NULL') THEN
         DATA_Looks_Good := TRUE;
      END IF;
   END Proc_TERM_CODE_CK;

   PROCEDURE Proc_FUND_CODE_CK( p3 IN VARCHAR2, DATA_Looks_Good IN OUT BOOLEAN )
   IS
--       L_FUND                     VARCHAR2(  6 CHAR);
      L_FUND_tmp                 VARCHAR2(  6 CHAR);
   BEGIN
      L_FUND := Nvl(p3, 'NULL');
      L_FUND_tmp := '#####';
      IF (Sp_Fund_Count > 0) THEN
         FOR s IN 1..Sp_Fund_Count LOOP
            IF (L_FUND = Special_Fund(s)) THEN
               L_FUND_tmp := L_FUND;
               EXIT;
            END IF;
         END LOOP;
      END IF;

      IF (L_FUND_tmp = '#####') THEN   -- ...not an exception, so check against rule table
         SELECT Max(FUND) FUND_Code INTO L_FUND
         FROM
               (
               SELECT RZBSCAM_FUND_CODE FUND FROM RZBSCAM WHERE RZBSCAM_FUND_CODE = L_FUND
            UNION
               SELECT '#####' FUND FROM dual
               );
      END IF;

      IF L_FUND != p3 THEN
         DATA_Looks_Good := DATA_Issue_33(3, Nvl(p3, 'NULL'));
      ELSE
         DATA_Looks_Good := TRUE;
      END IF;
   END Proc_FUND_CODE_CK;

   PROCEDURE Proc_RORMESG_Date_CK( p14 IN VARCHAR2, DATA_Looks_Good IN OUT BOOLEAN, Period OUT NUMBER )
   IS
      L_Special_Note             VARCHAR2( 25 CHAR);
      Number_of_Days             NUMBER := 30;
      L_MX_Str_Len               NUMBER := 25;
   BEGIN
      L_Special_Note := SubStr(Upper(Nvl(p14, 'N/A')), 1, L_MX_Str_Len);
      IF (L_Special_Note != 'N/A') THEN
         IF unm.rzkflib.is_numeric(L_Special_Note) THEN
            Number_of_Days := To_Number(L_Special_Note);
            IF ((Number_of_Days < 1) OR (Number_of_Days > 365)) THEN
               Number_of_Days  := -1;
               DATA_Looks_Good := DATA_Issue_33(14, L_Special_Note);
            ELSE
               DATA_Looks_Good := TRUE;
            END IF;
         ELSE
            Number_of_Days  := -1;
            DATA_Looks_Good := DATA_Issue_33(14, L_Special_Note);
         END IF;
      END IF;
      Period := Number_of_Days;
   END Proc_RORMESG_Date_CK;

   PROCEDURE Proc_RORMESG_CAT_CK( p15 IN VARCHAR2, DATA_Looks_Good IN OUT BOOLEAN )
   IS
      L_Special_Note             VARCHAR2( 25 CHAR);
      L_String                   VARCHAR2( 25 CHAR);
      L_MX_Str_Len               NUMBER  := 25;
   BEGIN
      L_Special_Note := SubStr(Upper(Nvl(p15, 'N/A')), 1, L_MX_Str_Len);
      IF (L_Special_Note != 'N/A') THEN
         SELECT Max(Cat_Code) INTO L_String
         FROM
               (
               SELECT RTVMESG_CODE Cat_Code FROM RTVMESG WHERE RTVMESG_CODE = L_Special_Note
            UNION
               SELECT '#####' Cat_Code FROM dual
               );

         IF (L_String != p15) THEN
            DATA_Looks_Good := DATA_Issue_33(15, Trim(SubStr(Nvl(p15, 'NULL'), 1, 5)));
         ELSE
            DATA_Looks_Good := TRUE;
         END IF;
      END IF;
   END Proc_RORMESG_CAT_CK;

   PROCEDURE Proc_RHRCOMM_CAT_CK( p12 IN VARCHAR2, DATA_Looks_Good IN OUT BOOLEAN )
   IS
      L_Special_Note             VARCHAR2( 25 CHAR);
      L_String                   VARCHAR2( 25 CHAR);
      L_MX_Str_Len               NUMBER  := 25;
   BEGIN
      L_Special_Note := SubStr(Upper(Nvl(p12, 'N/A')), 1, L_MX_Str_Len);
      IF (L_Special_Note != 'N/A') THEN
         SELECT Max(Cat_Code) INTO L_String
         FROM
               (
               SELECT RTVCCOM_CODE Cat_Code FROM RTVCCOM WHERE RTVCCOM_CODE = L_Special_Note
            UNION
               SELECT '#####' Cat_Code FROM dual
               );

         IF (L_String != p12) THEN
            DATA_Looks_Good := DATA_Issue_33(12, Trim(SubStr(Nvl(p12, 'NULL'), 1, 5)));
         ELSE
            DATA_Looks_Good := TRUE;
         END IF;
      END IF;
   END Proc_RHRCOMM_CAT_CK;

   PROCEDURE Proc_UNM_ID_CK( p2 IN VARCHAR2, DATA_Looks_Good IN OUT BOOLEAN )
   IS
      L_UNM_ID                   VARCHAR2( 11 CHAR);
   BEGIN
      L_UNM_ID := Nvl(p2, 'NULL');
      IF Length(L_UNM_ID) != 9 THEN
         DATA_Looks_Good := DATA_Issue_33(2, L_UNM_ID);
      ELSE
         SELECT Max(SPRIDEN_ID) INTO L_UNM_ID FROM SPRIDEN WHERE SPRIDEN_ID = p2;
         IF L_UNM_ID != p2 THEN
            DATA_Looks_Good := DATA_Issue_33(2, L_UNM_ID);
         ELSE
            DATA_Looks_Good := TRUE;
         END IF;
      END IF;
   END Proc_UNM_ID_CK;

   PROCEDURE Proc_AIDY_CK( p1 IN VARCHAR2, AIDY_OK IN OUT BOOLEAN, DATA_Looks_Good IN OUT BOOLEAN )
   IS
   BEGIN
      L_AIDY := Nvl(p1, 'N/A');
      IF ((Length(L_AIDY) != 4) OR (unm.rzkflib.is_numeric(p1) = FALSE)) THEN
         DATA_Looks_Good := DATA_Issue_33(1, Nvl(p1, 'NULL'));
      ELSE
         SELECT Max(RZBSCAM_AIDY_CODE) INTO L_AIDY FROM RZBSCAM WHERE RZBSCAM_AIDY_CODE = p1;
         IF ((L_AIDY != p1) OR (Current_AIDY > p1)) THEN
            DATA_Looks_Good := DATA_Issue_33(1, Nvl(p1, 'NULL'));
         ELSE
            DATA_Looks_Good := TRUE;
            L_AIDY  := p1;
            AIDY_OK := TRUE;
         END IF;
      END IF;
      IF (L_AIDY = 'N/A') THEN
         L_AIDY := 'NULL';
      END IF;
   END Proc_AIDY_CK;

   FUNCTION This_Is_A_Fund_Exception( p1 IN VARCHAR2, p3 IN VARCHAR2, e1 OUT NUMBER,
                                      DATA_Looks_Good IN OUT BOOLEAN ) RETURN BOOLEAN
   IS
      L_Special_Note             VARCHAR2( 25 CHAR);
      L_FUND                     VARCHAR2(  6 CHAR);
   BEGIN
      e1 := 0;
      L_Special_Note := Upper(Nvl(p1, 'N/A'));
      IF (L_Special_Note = 'FUNDEXCEPTION') THEN
         Fund_Ecptn_Cnt := Fund_Ecptn_Cnt + 1;
         L_FUND := Upper(Nvl(p3, 'NULL'));
         SELECT Max(FUND) FUND_Code INTO L_FUND
         FROM (SELECT RFRBASE_FUND_CODE FUND FROM RFRBASE WHERE RFRBASE_FUND_CODE = L_FUND
            UNION
               SELECT '#####' FUND FROM dual);

         IF (L_FUND = p3) THEN
            Sp_Fund_Count := Sp_Fund_Count + 1;
            Special_Fund(Sp_Fund_Count) := L_FUND;
         ELSE
            DATA_Looks_Good := DATA_Issue_33(15, Nvl(p3, 'NULL'));
            e1 := 1;
         END IF;
         L_ERROR_FIELD := 0;
         RETURN TRUE;
      ELSE
         RETURN FALSE;
      END IF;
   END This_Is_A_Fund_Exception;

PROCEDURE show_message_1( string IN VARCHAR2, flag IN BOOLEAN )
IS
BEGIN
   IF flag THEN
      Dbms_Output.Put_Line( string||'TRUE' );
   ELSE
      Dbms_Output.Put_Line( string||'FALSE' );
   END IF;
END show_message_1;

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
   FUNCTION f_Verify_Input_Data  RETURN NUMBER
   IS
      e1                         NUMBER  := 0;
      DATA_Looks_Good            BOOLEAN := TRUE;
      DATA_Issues                NUMBER  := 0;
      AIDY_OK                    BOOLEAN := FALSE;
      L_Special_Note             VARCHAR2( 25 CHAR);
   BEGIN
      L_Special_Note := Upper(Nvl(L_fld(3), 'N/A'));
      L_ERROR_FIELD := 0;
      IF This_Is_A_Fund_Exception( L_fld(1), L_fld(3), e1, DATA_Looks_Good ) THEN
         IF (DATA_Looks_Good = FALSE) THEN DATA_Issues := DATA_Issues + 1; END IF;
         RETURN e1;
      ELSIF (L_Special_Note = 'COMMENTONLY') THEN
         RETURN DATA_Issues;
      ELSE
         L_ERROR_FIELD := 1;
         Proc_AIDY_CK( L_fld(1), AIDY_OK, DATA_Looks_Good );
         IF (DATA_Looks_Good = FALSE) THEN DATA_Issues := DATA_Issues + 1; END IF;
         L_ERROR_FIELD := 2;
         Proc_UNM_ID_CK( L_fld(2), DATA_Looks_Good );
         IF (DATA_Looks_Good = FALSE) THEN DATA_Issues := DATA_Issues + 1; END IF;
         L_ERROR_FIELD := 3;
         Proc_FUND_CODE_CK( L_fld(3), DATA_Looks_Good );
         IF (DATA_Looks_Good = FALSE) THEN DATA_Issues := DATA_Issues + 1; END IF;
         L_ERROR_FIELD := 4;
         Proc_TERM_CODE_CK( L_fld(1), AIDY_OK, L_fld(4), DATA_Looks_Good );
         IF (DATA_Looks_Good = FALSE) THEN DATA_Issues := DATA_Issues + 1; END IF;
         L_ERROR_FIELD := 5;
         Proc_AMOUNT_CK( L_fld(5), DATA_Looks_Good );
         IF (DATA_Looks_Good = FALSE) THEN DATA_Issues := DATA_Issues + 1; END IF;
         L_ERROR_FIELD := 6;
         Proc_ACTION_CK( L_fld(6), DATA_Looks_Good );
         IF (DATA_Looks_Good = FALSE) THEN DATA_Issues := DATA_Issues + 1; END IF;
         IF (Field_Count > 6) THEN
            L_ERROR_FIELD := 7;
            Proc_AWRD_LTR_CK( L_fld(7), DATA_Looks_Good );
            IF (DATA_Looks_Good = FALSE) THEN DATA_Issues := DATA_Issues + 1; END IF;
         END IF;
         IF (Field_Count > 7) THEN
            L_ERROR_FIELD := 8;
            Proc_ACAD_LD_CK( L_fld(8), DATA_Looks_Good, G_Academic_Load );
            IF (DATA_Looks_Good = FALSE) THEN DATA_Issues := DATA_Issues + 1; END IF;
         END IF;
         G_RRRAREQ_TREQ_CODE := 'x';
         G_RRRAREQ_TRST_CODE := 'x';
         indx := 9;
         IF (Field_Count >= indx) THEN
            G_RRRAREQ_TREQ_CODE := Nvl(L_fld(indx), 'x');
            indx := indx + 1;
            IF (Field_Count >= indx) THEN
               G_RRRAREQ_TRST_CODE := Nvl(L_fld(indx), 'x');
               indx := indx + 1;
               IF (Field_Count >= indx) THEN
                  G_RRRAREQ_PERIOD := L_fld(indx);
               END IF;
            END IF;
            G_AWARD_AIDY  := L_fld(1);
            G_UNM_BanID   := L_fld(2);
            G_FUND_Code   := L_fld(3);
            G_Banner_PIDM := gb_common.f_get_pidm( G_UNM_BanID );
            Analysis_Code := 0;
            IF ((G_RRRAREQ_TREQ_CODE != 'x') AND (G_RRRAREQ_TRST_CODE != 'x')) THEN
               Analysis_Code := unm.rzkflib.f_RRRAREQ_Data_OK( Analysis_Note, G_Banner_PIDM, G_AWARD_AIDY, G_FUND_Code, G_RRRAREQ_TREQ_CODE, G_RRRAREQ_TRST_CODE, G_RRRAREQ_PERIOD );
               IF (Analysis_Code < 1) THEN
                  IF (Analysis_Code = 0) THEN
                     Analysis_Note := G_RRRAREQ_TREQ_CODE;
                     DATA_is_OK    := DATA_Issue_33(9, Analysis_Note );
                  ELSIF (Analysis_Code = -1) THEN
                     Analysis_Note := G_RRRAREQ_PERIOD;
                     DATA_is_OK    := DATA_Issue_33(10, Analysis_Note );
                  ELSIF (Analysis_Code = -2) THEN
                     Analysis_Note := G_RRRAREQ_TRST_CODE;
                     DATA_is_OK    := DATA_Issue_33(11, Analysis_Note );
                  ELSIF (Analysis_Code = -3) THEN
                     Analysis_Note := 'NULL';
                     DATA_is_OK    := DATA_Issue_33(11, Analysis_Note );
                  END IF;
                  DATA_Issues   := DATA_Issues + 1;
                  DATA_Looks_Good := FALSE;
               END IF;
            ELSIF ((G_RRRAREQ_TREQ_CODE != 'x') AND (G_RRRAREQ_TRST_CODE = 'x')) THEN
               Analysis_Note := 'NULL';
               DATA_is_OK    := DATA_Issue_33(11, Analysis_Note );
               DATA_Issues   := DATA_Issues + 1;
               DATA_Looks_Good := FALSE;
            END IF;
         END IF;
         IF (Field_Count > 11) THEN
            L_ERROR_FIELD := 12;
            Proc_RHRCOMM_CAT_CK( L_fld(12), DATA_Looks_Good );
            IF (DATA_Looks_Good = FALSE) THEN DATA_Issues := DATA_Issues + 1; END IF;
            IF (Field_Count > 13) THEN
               L_ERROR_FIELD := 14;
               Proc_RORMESG_Date_CK( L_fld(14), DATA_Looks_Good, G_RORMESG_Date_Period );
               IF (DATA_Looks_Good = FALSE) THEN DATA_Issues := DATA_Issues + 1; END IF;
               IF (Field_Count > 14) THEN
                  L_ERROR_FIELD := 15;
                  Proc_RORMESG_CAT_CK( L_fld(15), DATA_Looks_Good );
                  IF (DATA_Looks_Good = FALSE) THEN DATA_Issues := DATA_Issues + 1; END IF;
               END IF;
            END IF;
         END IF;
      END IF;  -- end of: IF This_Is_A_Fund_Exception(...) THEN ... ELSE

      IF DATA_Looks_Good THEN
         IF ((L_AIDY = L_fld(1)) AND (L_TERM = L_fld(4))) THEN
            TERM_Award_Count := TERM_Award_Count + 1;
         ELSIF ((L_AIDY = L_fld(1)) AND (L_TERM = 'NULL')) THEN
            AIDY_Award_Count := AIDY_Award_Count + 1;
         END IF;
      END IF;

      L_ERROR_FIELD := 0;
      RETURN DATA_Issues;

   EXCEPTION
      WHEN OTHERS THEN
         Dbms_Output.Put_Line( 'f_Verify_Input_Data() ==> L_ERROR_FIELD: '||L_ERROR_FIELD||'   ...Field_Count: '||Field_Count );
         unm.rzkflib.Track_Errors;
         IF ((L_ERROR_FIELD >= 1) AND (L_ERROR_FIELD <= 13))THEN
            Append_Error_Note(L_fld_errdata(L_ERROR_FIELD), Nvl(L_fld(L_ERROR_FIELD), 'NULL'), L_fld_unqerr(L_ERROR_FIELD));
         ELSIF (L_ERROR_FIELD = 14)THEN
            Append_Error_Note(L_fld_errdata(L_ERROR_FIELD), L_fld(1)||'~'||L_fld(4), L_fld_unqerr(L_ERROR_FIELD));
         END IF;
         IF DATA_Looks_Good THEN
            IF ((L_AIDY = L_fld(1)) AND (L_TERM = L_fld(4))) THEN
               TERM_Award_Count := TERM_Award_Count + 1;
            ELSIF ((L_AIDY = L_fld(1)) AND (L_TERM = 'NULL')) THEN
               AIDY_Award_Count := AIDY_Award_Count + 1;
            END IF;
         END IF;

         RETURN DATA_Issues;
   END f_Verify_Input_Data;

   FUNCTION FMT_INT( numb IN NUMBER, Nmx IN NUMBER )
      RETURN VARCHAR2
   IS
      Fmtd_Intgr        VARCHAR2( 15 CHAR );
      Format_Mask       VARCHAR2( 15 CHAR ) := '999,999,990';
      Max_Digits        NUMBER;
      i                 NUMBER;

   BEGIN
      Max_Digits  := Length(Trim(To_Char(Nmx, '9999999')));
      Format_Mask := '0';
      FOR i IN 2..Max_Digits LOOP
         IF (Mod(i, 4) = 0) THEN
            Format_Mask := ','||Format_Mask;
         END IF;
         Format_Mask := '9'||Format_Mask;
      END LOOP;

      Fmtd_Intgr := To_Char( numb, Format_Mask );
      RETURN Fmtd_Intgr;
   END FMT_INT;

   FUNCTION Extract_Awarding_Data( Download_Data IN VARCHAR2 )
      RETURN VARCHAR2
   IS
      TC          NUMBER := 0;
      TP          NUMBER := 0;
   BEGIN
      FOR i IN 1..Length(Download_Data) LOOP
         IF (SubStr(Download_Data, i, 1) = CHR(9)) THEN
            TC := TC + 1;
            TP := i;
         EXIT WHEN (TC > 5);
         END IF;
      END LOOP;
      IF (TC > 5) THEN
         RETURN SubStr(Download_Data, 1, TP-1);
      ELSE
         RETURN Download_Data;
      END IF;
   END Extract_Awarding_Data;

   FUNCTION f_get_pidm( UNM_ID IN spriden.spriden_id%TYPE )
         RETURN spriden.spriden_pidm%TYPE
      IS
         v_return   NUMBER;
      BEGIN
         v_return := gb_common.f_get_pidm( UNM_ID );
         RETURN v_return;

      EXCEPTION
         WHEN OTHERS THEN
            v_return := NULL;
            RETURN v_return;
   END f_get_pidm;

   FUNCTION RTrim_CTRL( string IN VARCHAR2 ) RETURN VARCHAR2
   IS
      rtn_value      VARCHAR2(1000 CHAR);
      idx            NUMBER;
   BEGIN
      IF string IS NULL THEN
         rtn_value := NULL;
      ELSIF (Length(string) = 0) THEN
         rtn_value := string;
      ELSE
-- Dbms_Output.Put_Line( 'string ==>'||string||'<<' );
         rtn_value := '';
         idx  := Length(string);
         FOR i IN 0..Length(string) LOOP
            IF Ascii(SubStr(string, idx-i, 1)) >= 32 THEN
               rtn_value := SubStr(string, 1, idx-i);
               EXIT;
            END IF;
         END LOOP;
         rtn_value := RTrim(rtn_value);
-- Dbms_Output.Put_Line( 'now at ==>'||rtn_value||'<<' );
      END IF;
      RETURN rtn_value;
   END RTrim_CTRL;

BEGIN
   IF Banner_One_Up_Num IS NULL THEN
      SELECT To_Number(To_Char(SYSDATE, 'DHH24MISS'))
      INTO One_Up_Number
      FROM dual;
   ELSIF (Banner_One_Up_Num < 0) THEN
      Internal_Version := f_api_version;
      RETURN Internal_Version;
   ELSE
      One_Up_Number := Banner_One_Up_Num;
   END IF;

   SELECT NAME
   INTO Instance_Name
   FROM v$database;

   SELECT Upper(SubStr(USERNAME, 1, 50))
   INTO User_Name
   FROM v$session s
   WHERE USERNAME IS NOT NULL
     AND AUDSID = UserEnv('sessionid');

   Current_AIDY := RZKFLIB.The_Period_Today('AIDY');
   Current_TERM := RZKFLIB.The_Period_Today('TERM');

   <<User_Name_OK_Block>>
   BEGIN
      IF (User_Name != 'FINAIDAPPWORX') THEN
         G_BASE_filename  := 'SFAO_SCH_LOAD_'||User_Name;
         G_INPUT_filename := G_BASE_filename ||G_file_extention;
         SELECT Count(*) INTO cnt FROM RZRSCIN;
         ROW_Count := cnt;
         Internal_Version := f_api_version;
         Table_Insert_Count := 0;
      END IF;

      Dbms_Output.new_line;
      Dbms_Output.put_line( '=======================================================');
      Dbms_Output.put_line( 'Load Scholarship Parameters; Version: '||Internal_Version );
      Dbms_Output.put_line( '...Executed by: ' || User_Name || ' in dB Instace: ' || Instance_Name );
      IF (User_Name = 'FINAIDAPPWORX') THEN
         Unathorized_User := 1;
         Dbms_Output.put_line( '   "FINAIDAPPWORX" Not Authorized to Run this Program.' );
         Dbms_Output.Put_Line( '       ...No further processing.' );
      ELSE
         Dbms_Output.put_line( '   ...The INPUT Filename is '||G_INPUT_filename         );
         IF (ROW_Count = 1) THEN
            Dbms_Output.put_line( '   ...There is 1 row of data in RZRSCIN'  );
         ELSE
            Dbms_Output.put_line( '   ...There are ' || ROW_Count || ' rows of data in RZRSCIN'  );
         END IF;
         Dbms_Output.put_line( '   ...Current_AIDY: '||Current_AIDY||' and Current_TERM: '||Current_TERM );
      END IF;
      Dbms_Output.put_line( '=======================================================');
      Dbms_Output.new_line;

      G_Message_Body := '=======================================================';
      G_Message_Body := G_Message_Body||CrLf||'Load Scholarship Parameters; Version: '||Internal_Version ;
      G_Message_Body := G_Message_Body||CrLf||'...Executed by: ' || User_Name || ' in dB Instance: ' || Instance_Name;
      G_Message_Body := G_Message_Body||CrLf||'...Executed on: ' || To_Char(SYSDATE, 'MM-DD-YYYY')||' at '||To_Char(SYSDATE, 'HH24:MI:SS');
      IF (User_Name = 'FINAIDAPPWORX') THEN
         G_Message_Body := G_Message_Body||CrLf||'   "FINAIDAPPWORX" Not Authorized to Run this Program.';
         G_Message_Body := G_Message_Body||CrLf||'       ...No further processing.';
      ELSE
         G_Message_Body := G_Message_Body||CrLf||'   ...The INPUT Filename is '||G_INPUT_filename;
         IF (ROW_Count = 1) THEN
            G_Message_Body := G_Message_Body||CrLf||'   ...There is 1 row of data in RZRSCIN';
         ELSE
            G_Message_Body := G_Message_Body||CrLf||'   ...There are ' || ROW_Count || ' rows of data in RZRSCIN';
         END IF;
         G_Message_Body := G_Message_Body||CrLf||'   ...Current_AIDY: '||Current_AIDY||' and Current_TERM: '||Current_TERM;
      END IF;
      G_Message_Body := G_Message_Body||CrLf||'======================================================='||CrLf;

      <<READ_Data_Block>>
      BEGIN
         IF (User_Name != 'FINAIDAPPWORX') THEN
            Utl_File.FGETATTR( G_file_directory, G_INPUT_filename, exists_flag, file_length, blocksize );
            IF (exists_flag = TRUE) THEN
               Data_Import_File_Exists := TRUE;
            ELSE
               Dbms_Output.Put_Line( ' Scholarship Import Data file does not exist. No further processing.' );
               Dbms_Output.Put_Line( '   File_Directory: '||  G_file_directory );
               Dbms_Output.Put_Line( '  Input_File_Name: '||  G_INPUT_filename );
               Dbms_Output.Put_Line( '      exists_flag: '||           'FALSE' );
               G_Message_Body := G_Message_Body||CrLf||' Scholarship Import Data file does not exist. No further processing.';
               G_Message_Body := G_Message_Body||CrLf||'~~~  ~~~  ~~~  ~~~  ~~~  ~~~  ~~~  ~~~';
               G_Message_Body := G_Message_Body||CrLf||'   File_Directory: '||  G_file_directory;
               G_Message_Body := G_Message_Body||CrLf||' Input_File_Name: '||  G_INPUT_filename ;
               G_Message_Body := G_Message_Body||CrLf||'      exists_flag: '||           'FALSE';
               G_Message_Body := G_Message_Body||CrLf||'======================================================='||CrLf;
               Data_Import_File_Exists := FALSE;
            END IF;

            IF Data_Import_File_Exists THEN
               G_INPUT_handle  := Utl_File.fopen(G_file_directory, G_INPUT_filename,   'R');
               File_Read_Count := 0;
               Total_Field_ERRORS := 0;
               AIDY_Award_Count   := 0;
               TERM_Award_Count   := 0;
               char_trkr_cnt      := 0;
               char_trkr_row_cnt  := 0;
               FOR n IN 1..MAX_Fields LOOP
                  L_fld(n)         := '';
                  L_fld_desc(n)    := '';
                  L_fld_errdata(n) := '';
                  L_fld_err(n)     := 0;
                  L_fld_unqerr(n)  := 0;
               END LOOP;
               L_fld_desc( 1) := ' AIDY Code column...........:';     L_fld_MSG_Order( 1) := 19;
               L_fld_desc( 2) := ' UNM_ID column..............:';     L_fld_MSG_Order( 2) :=  1;
               L_fld_desc( 3) := ' FUND Code column...........:';     L_fld_MSG_Order( 3) :=  2;
               L_fld_desc( 4) := ' TERM Code column...........:';     L_fld_MSG_Order( 4) :=  3;
--                L_fld_desc( 5) := ' AMOUNT column..............:';     L_fld_MSG_Order( 5) :=  4;
               L_fld_desc( 5) := ' AMOUNT non-Numeric.........:';     L_fld_MSG_Order( 5) :=  4;
               L_fld_desc(19) := ' AMOUNT Exceeds Fund Max-Min:';     L_fld_MSG_Order(19) :=  5;
               L_fld_desc( 6) := ' ACTION column..............:';     L_fld_MSG_Order( 6) :=  6;
               L_fld_desc( 7) := ' AWRD Letter Indicator......:';     L_fld_MSG_Order( 7) :=  7;
               L_fld_desc( 8) := ' Academic Load..............:';     L_fld_MSG_Order( 8) :=  8;
               L_fld_desc( 9) := ' RRRAREQ Period Ineligible..:';     L_fld_MSG_Order( 9) :=  9;
               L_fld_desc(10) := ' RRRAREQ Period Invalid.....:';     L_fld_MSG_Order(10) := 10;
               L_fld_desc(11) := ' RRRAREQ TRST Issue.........:';     L_fld_MSG_Order(11) := 11;
               L_fld_desc(12) := ' RHRCOMM Category Code......:';     L_fld_MSG_Order(12) := 12;
               L_fld_desc(13) := ' RHRCOMM Message............:';     L_fld_MSG_Order(13) := 13;
               L_fld_desc(14) := ' RORMESG Period.............:';     L_fld_MSG_Order(14) := 14;
               L_fld_desc(15) := ' RORMESG Message Code.......:';     L_fld_MSG_Order(15) := 15;
               L_fld_desc(16) := ' RORMESG Message............:';     L_fld_MSG_Order(16) := 16;
               L_fld_desc(17) := ' AIDY~TERM use..............:';     L_fld_MSG_Order(17) := 17;
               L_fld_desc(18) := ' FUND Exception.............:';     L_fld_MSG_Order(18) := 18;
               FOR n IN 1..char_trkr_max_cnt LOOP
                  char_trkr(n) := 0;
               END LOOP;
--==============================================================================
--==============================================================================
   IF (Instance_Name = 'DEVL') THEN
      G_Message_Body := G_Message_Body||CrLf;
      G_Message_Body := G_Message_Body||'~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~'||CrLf;
      G_Message_Body := G_Message_Body||'~~~~~  Input data displayed (DEVL Only) for reference   ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~'||CrLf;
      G_Message_Body := G_Message_Body||'~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~';
   END IF;
--==============================================================================
--==============================================================================
               LOOP
                  <<Data_Row_Count_Block>>
                  BEGIN
                     Utl_File.GET_LINE(G_INPUT_handle, Download_Data);
                     Download_Data := RTrim_CTRL( Download_Data );
/*
                     n := Length(Download_Data);
                     IF (n > 1) THEN
                        IF (ASCII(SubStr(Download_Data, 1, n)) < 32) THEN
                           Download_Data := SubStr(Download_Data, 1, n-1);
                        END IF;
                     END IF;
*/
                     tmpNumber := Nvl(Length(REPLACE(Download_Data, Chr(9), '')), 0);
                     <<Data_Read_Not_Blank_Block>>
                     BEGIN
                        IF (tmpNumber > 1) THEN
                           Download_Data   := Trim(Download_Data);
                           File_Read_Count := File_Read_Count + 1;
                           Awarding_Data := Extract_Awarding_Data( Download_Data );
                           IF (Unallowed_Character_Input( Awarding_Data ) = TRUE) THEN
                              Total_Field_ERRORS := Total_Field_ERRORS + 1;
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
                              G_UNM_BanID         := NULL;
                              G_AWARD_AIDY        := NULL;
                              G_AWARD_TERM        := NULL;
                              G_FUND_Code         := NULL;
                              G_AWARD_Amount      := NULL;
                              G_AWARD_Action      := NULL;
                              G_AWRD_LTR_IND      := NULL;
                              G_Academic_Load     := NULL;
                              G_RHRCOMM_Cat_Code  := NULL;
                              G_Text_for_RHRCOMM  := NULL;
                              G_EXDY_for_RORMESG  := NULL;
                              G_CODE_for_RORMESG  := NULL;
                              G_Text_for_RORMESG  := NULL;
                              G_RRRAREQ_TREQ_CODE := NULL;
                              G_RRRAREQ_TRST_CODE := NULL;
                              G_RRRAREQ_PERIOD    := NULL;
                              L_fld( 1) := NULL;
                              L_fld( 2) := NULL;
                              L_fld( 3) := NULL;
                              L_fld( 4) := NULL;
                              L_fld( 5) := NULL;
                              L_fld( 6) := NULL;
                              L_fld( 7) := NULL;
                              L_fld( 8) := NULL;
                              L_fld( 9) := NULL;
                              L_fld(10) := NULL;
                              L_fld(11) := NULL;
                              L_fld(12) := NULL;
                              L_fld(13) := NULL;
                              L_fld(14) := NULL;
                              L_fld(15) := NULL;
                              L_fld := unm.ArrayTables.f_Parse_String( Download_Data, HT, Field_Count );
-- FOR h IN 1..Field_Count LOOP
--    Dbms_Output.Put_Line( '~~~~~['||Field_Count||']~~~~~ L_fld('||To_Char(h, '90')||') ==>'||SubStr(L_fld(h), 1, 35)||'<==' );
-- END LOOP;

                              Total_Field_ERRORS := Total_Field_ERRORS + f_Verify_Input_Data;
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
                              tmpString := Upper(Nvl(L_fld(1), 'N/A'));
                              IF (tmpString != 'FUNDEXCEPTION') THEN
                                 Total_Param_Data_ROWS := Total_Param_Data_ROWS + 1;
                                 tmpString2 := Upper(Nvl(L_fld(3), 'N/A'));
                                 IF (tmpString2 != 'COMMENTONLY') THEN
                                    Param_Data_ROWS_with_Award := Param_Data_ROWS_with_Award + 1;
                                 ELSE
                                    Total_Comment_ROWS := Total_Comment_ROWS + 1;
                                 END IF;
                                 <<Data_Row_Award_Block>>
                                 BEGIN
                                    IF (tmpString2 != 'COMMENTONLY') THEN
                                       indx := 7;
                                       IF (Field_Count >= indx) THEN
                                          AWRD_LTR_IND_Flags := AWRD_LTR_IND_Flags + 1;
                                          IF (L_fld(indx) IS NOT NULL) THEN
                                             IF (Upper(L_fld(indx)) = 'Y') THEN
                                                AWRD_LTR_IND_Yes := AWRD_LTR_IND_Yes + 1;
                                             ELSIF (Upper(L_fld(indx)) = 'N') THEN
                                                AWRD_LTR_IND_No := AWRD_LTR_IND_No + 1;
                                             END IF;
                                          ELSE
                                             AWRD_LTR_IND_NULLs := AWRD_LTR_IND_NULLs + 1;
                                          END IF;
                                       ELSE
                                          AWRD_LTR_IND_NULLs := AWRD_LTR_IND_NULLs + 1;
                                       END IF;
                                       indx := 8;
                                       IF (Field_Count >= indx) THEN
                                          Proc_ACAD_LD_CK( L_fld(indx), DATA_is_OK, G_Academic_Load );
                                          PCKG_LOAD_IND := PCKG_LOAD_IND + 1;
                                          IF (L_fld(indx) IS NOT NULL) THEN
                                             IF DATA_is_OK THEN
                                                PCKG_LOAD_OK := PCKG_LOAD_OK + 1;
                                             END IF;
                                          ELSE
                                             PCKG_LOAD_NULLs := PCKG_LOAD_NULLs + 1;
                                          END IF;
                                       ELSE
                                          PCKG_LOAD_NULLs := PCKG_LOAD_NULLs + 1;
                                       END IF;

                                       G_RRRAREQ_TREQ_CODE := 'x';
                                       G_RRRAREQ_TRST_CODE := 'x';
                                       indx := 9;
                                       IF (Field_Count >= indx) THEN
                                          G_RRRAREQ_TREQ_CODE := Nvl(L_fld(indx), 'x');
                                          indx := indx + 1;
                                          IF ((G_RRRAREQ_TREQ_CODE != 'x') AND (Field_Count >= indx)) THEN
                                             G_RRRAREQ_TRST_CODE := Nvl(L_fld(indx), 'x');
                                             indx := indx + 1;
                                             IF ((G_RRRAREQ_TRST_CODE != 'x') AND (Field_Count >= indx)) THEN
                                                G_RRRAREQ_PERIOD := L_fld(indx);
                                             END IF;
                                          END IF;
                                          G_AWARD_AIDY  := L_fld(1);
                                          G_UNM_BanID   := L_fld(2);
                                          G_FUND_Code   := L_fld(3);
                                          G_Banner_PIDM := gb_common.f_get_pidm( G_UNM_BanID );
                                          Analysis_Code := 0;
                                          IF ((G_RRRAREQ_TREQ_CODE != 'x') AND (G_RRRAREQ_TRST_CODE != 'x')) THEN
                                             Analysis_Code := unm.rzkflib.f_RRRAREQ_Data_OK( Analysis_Note, G_Banner_PIDM, G_AWARD_AIDY, G_FUND_Code, G_RRRAREQ_TREQ_CODE, G_RRRAREQ_TRST_CODE, G_RRRAREQ_PERIOD );
                                             IF (Analysis_Code = 0) THEN
                                                RRRAREQ_Load_NoGo := RRRAREQ_Load_NoGo + 1;
                                             ELSE
                                                RRRAREQ_Load_OK := RRRAREQ_Load_OK + 1;
                                             END IF;
                                          ELSE
                                             G_RRRAREQ_TREQ_CODE := NULL;
                                             G_RRRAREQ_TRST_CODE := NULL;
                                             G_RRRAREQ_PERIOD    := NULL;
                                             RRRAREQ_Load_NULLs  := RRRAREQ_Load_NULLs + 1;
                                          END IF;
                                       END IF;
                                    END IF;     -- end of: IF (tmpString2 != 'COMMENTONLY') THEN
--==============================================================================
--==============================================================================
-- ==  Improper to try using "FUNDEXCEPTION" and "COMMENTONLY" in same data line
   IF (Instance_Name = 'DEVL') THEN
      tmpString3 := RPad(L_fld( 1), Length('FUNDEXCEPTION'))||'  '||RPad(L_fld( 2),  9)||
              '  '||RPad(L_fld( 3), Length('COMMENTONLY'))  ||'  '||RPad(Nvl(L_fld( 4), ' '), 9)||
              '  '||LPad(L_fld( 5), 10)||'  '||RPad(L_fld( 6),  5);
      IF (Field_Count >=  7) THEN tmpString3 := tmpString3||'  '||RPad(L_fld( 7),  2);END IF;
      IF (Field_Count >=  8) THEN tmpString3 := tmpString3||'  '||RPad(L_fld( 8),  2);END IF;
      IF (Field_Count >=  9) THEN tmpString3 := tmpString3||'  '||RPad(L_fld( 9),  6);END IF;
      IF (Field_Count >= 10) THEN tmpString3 := tmpString3||'  '||RPad(L_fld(10),  2);END IF;
      IF (Field_Count >= 11) THEN tmpString3 := tmpString3||'  '||RPad(L_fld(11),  6);END IF;
      Dbms_Output.Put_Line( 'Data ['||To_Char(File_Read_Count, '90')||'] ==>>'||Trim(tmpString3)||'<<' );
      G_Message_Body := G_Message_Body||CrLf||'Data ['||To_Char(File_Read_Count, '90')||'] ==>>'||Trim(tmpString3)||'<<';
   END IF;
--==============================================================================
--==============================================================================
                                 END Data_Row_Award_Block;

                                 indx := 12;
                                 IF (Field_Count >= indx) THEN
                                    IF ((Nvl(L_fld(indx), 'x')||Nvl(L_fld(indx+1), 'x')) != 'xx') THEN
                                       RHACOMM_Data := RHACOMM_Data + 1;
                                       IF (tmpString2 = 'COMMENTONLY') THEN
                                          RHACOMM_No_Award := RHACOMM_No_Award + 1;
                                       ELSE
                                          RHACOMM_with_Award := RHACOMM_with_Award + 1;
                                       END IF;
                                    END IF;
                                 END IF;
                                 indx := 14;
                                 IF (Field_Count >= indx) THEN
                                    IF ((Nvl(L_fld(indx), 'y')||Nvl(L_fld(indx+1), 'y')||Nvl(L_fld(indx+2), 'y')) != 'yyy') THEN
                                       RORMESG_Data := RORMESG_Data + 1;
                                       IF (tmpString2 = 'COMMENTONLY') THEN
                                          RORMESG_No_Award := RORMESG_No_Award + 1;
                                       ELSE
                                          RORMESG_with_Award := RORMESG_with_Award + 1;
                                       END IF;
                                    END IF;
                                 END IF;
                                 indx := 12;
                                 IF (Field_Count >= indx) THEN
                                    IF (Nvl(L_fld(indx), 'x') != 'x') THEN
                                       RHACOMM_Cat_Codes := RHACOMM_Cat_Codes + 1;
                                    END IF;
                                 END IF;
                              ELSE
--==============================================================================
--==============================================================================
-- ==  Improper to try using "FUNDEXCEPTION" and "COMMENTONLY" in same data line
   IF (Instance_Name = 'DEVL') THEN
      tmpString3 := RPad(L_fld( 1), Length('FUNDEXCEPTION'))||'  '||RPad(Nvl(L_fld( 2), ' '), 9)||
              '  '||RPad(L_fld( 3), Length('COMMENTONLY'));
      Dbms_Output.Put_Line( 'AWARD Data ['||To_Char(File_Read_Count, '90')||'] ==>>'||Trim(tmpString3)||'<<' );
      G_Message_Body := G_Message_Body||CrLf||'Data ['||To_Char(File_Read_Count, '90')||'] ==>>'||Trim(tmpString3)||'<<';
   END IF;
--==============================================================================
--==============================================================================
                              END IF;     -- end of: IF (tmpString != 'FUNDEXCEPTION') THEN
                           END IF;     -- end of: IF (Unallowed_Character_Input( Awarding_Data ) = TRUE) THEN
                        END IF;     -- end of: IF (tmpNumber > 1) THEN
                     END Data_Read_Not_Blank_Block;

                  EXCEPTION
                     WHEN NO_DATA_FOUND THEN
                        EXIT;
                     WHEN OTHERS THEN
                        IF SQLCODE != -29284 THEN
                           Dbms_Output.Put_Line( '['||File_Read_Count||'] Exception in Data_Row_Count_Block -- ' );
                           unm.rzkflib.Track_Errors;
                        END IF;
                        File_Read_ERRORS := File_Read_ERRORS + 1;
                  END Data_Row_Count_Block;
               END LOOP;
--==============================================================================
--==============================================================================
   IF (Instance_Name = 'DEVL') THEN
      G_Message_Body := G_Message_Body||CrLf||'~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~'||CrLf;
   END IF;
--==============================================================================
--==============================================================================

               Total_Field_ERRORS := 0;
               FOR idx IN 1..MAX_Fields LOOP
                  Total_Field_ERRORS := Total_Field_ERRORS + L_fld_err(idx);
               END LOOP;

               IF (Total_Field_ERRORS > 0) THEN
                  OK_to_Process_Input_Data := FALSE;
                  OK_to_Call_RZKSCLD := FALSE;
                  OK_to_Write_Notes_Only := FALSE;
                  Digit_Count := Length(To_Char(Trunc(File_Read_Count)));
                  G_Message_Body := G_Message_Body||CrLf||'=======================================================';
                  G_Message_Body := G_Message_Body||CrLf||'There were '||File_Read_Count||' INPUT data rows read and analyzed.';
                  G_Message_Body := G_Message_Body||CrLf||'    '||LPad(To_Char(Fund_Ecptn_Cnt),        Digit_Count)||' SFAO non-Special-Rule Restricted awards';
                  G_Message_Body := G_Message_Body||CrLf||'    '||LPad(To_Char(Total_Param_Data_ROWS), Digit_Count)||' total Input Parameter Rows';
                  G_Message_Body := G_Message_Body||CrLf||'    '||LPad(To_Char(Total_Comment_ROWS),    Digit_Count)||' total Input Parameter Rows for Comments ONLY';
                  Nmx := Total_Field_ERRORS ;
                  IF (Total_Field_ERRORS = 1) THEN
                     G_Message_Body := G_Message_Body||CrLf||'Data Issue Identified in 1 row of the input file --';
                     Dbms_Output.Put_Line( 'Data Issue Identified in 1 row of the input file --' );
                  ELSE
                     G_Message_Body := G_Message_Body||CrLf||'Data Issues Identified in multiple rows of the input file --';
                     Dbms_Output.Put_Line( 'Data Issues Identified in multiple rows of the input file --' );
                  END IF;
                  G_Message_Body := G_Message_Body||CrLf||   '-------------------------------------------------------';
                  IF ( char_trkr_cnt > 0 ) THEN
                     IF (char_trkr_row_cnt = 1) THEN
                        G_Message_Body := G_Message_Body||CrLf||' ...Unallowed Character(s) ['||char_trkr_cnt||'] Found in '||char_trkr_row_cnt||' row: ';
                     ELSE
                        G_Message_Body := G_Message_Body||CrLf||' ...Unallowed Character(s) ['||char_trkr_cnt||'] Found in '||char_trkr_row_cnt||' rows: ';
                     END IF;
                     IF (char_trkr_cnt > char_trkr_max_cnt) THEN
                        tmpNumber := char_trkr_max_cnt;
                     ELSE
                        tmpNumber := char_trkr_cnt;
                     END IF;
                     FOR n IN 1..tmpNumber LOOP
                        IF (char_trkr(n) > 0) THEN
                           G_Message_Body := G_Message_Body||CrLf||' ............ ['||To_Char(char_trkr(n), '990')||'] -- '||
                                             unm.rzkflib.ASCII_7_Description(char_trkr(n));
                        END IF;
                     END LOOP;
                     G_Message_Body := G_Message_Body||CrLf||' ........only these are allowed: 1234567890.ABCDEFGHIJKLMNOPQRSTUVWXYZ';
                     G_Message_Body := G_Message_Body||CrLf||'              [except in the text fields for RHACOMM or RORMESG]';
                     G_Message_Body := G_Message_Body||CrLf||'-------------------------------------------------------';
                  END IF;
                  FOR g IN 1..MAX_Fields LOOP
                     f := L_fld_MSG_Order(g);
                     IF (L_fld_err(f) > 0) THEN
                        G_Message_Body := G_Message_Body||CrLf||L_fld_desc(f)||FMT_INT(L_fld_err(f), Nmx)||' total row(s), '||L_fld_unqerr(f)||' unique value(s).';
                        G_Message_Body := G_Message_Body||CrLf||' ........value(s): '||L_fld_errdata(f);
                        Dbms_Output.Put_Line( L_fld_desc(f)||FMT_INT(L_fld_err(f), Nmx)||' total row(s), '||L_fld_unqerr(f)||' unique value(s).' );
                        Dbms_Output.Put_Line( ' ........value(s): '||L_fld_errdata(f) );
                     END IF;
                  END LOOP;
                  Dbms_Output.Put_Line( '=======================================================' );
                  Dbms_Output.Put_Line( '*******  Data Issues in the Input File  *******' );
                  Dbms_Output.Put_Line( 'Your request cannot be processed now; fix the data and'  );
                  Dbms_Output.Put_Line( 'try again later.' );
                  Dbms_Output.Put_Line( '=======================================================' );
                  G_Message_Body := G_Message_Body||CrLf;
                  G_Message_Body := G_Message_Body||CrLf||'-------------------------------------------------------';
                  G_Message_Body := G_Message_Body||CrLf||'***********  Data Issues in the Input File  ***********';
                  G_Message_Body := G_Message_Body||CrLf||'Your request cannot be processed now; fix the data and';
                  G_Message_Body := G_Message_Body||CrLf||'try again later.';
                  G_Message_Body := G_Message_Body||CrLf||'=======================================================';
               ELSE
                  IF ((Param_Data_ROWS_with_Award > 0) AND (ROW_Count = 0)) THEN
                     OK_to_Call_RZKSCLD := TRUE;
                  ELSE
                     OK_to_Call_RZKSCLD := FALSE;
                  END IF;
                  IF (Total_Comment_ROWS = Total_Param_Data_ROWS) THEN
                     OK_to_Write_Notes_Only := TRUE;
                  ELSE
                     OK_to_Write_Notes_Only := FALSE;
                  END IF;
                  IF (OK_to_Call_RZKSCLD OR OK_to_Write_Notes_Only) THEN
                     OK_to_Process_Input_Data := TRUE;
                  END IF;
                  IF (Param_Data_ROWS_with_Award > 0) THEN
                     IF (TERM_Award_Count = File_Read_Count) THEN
                        pct_TERM := '100.00';
                        pct_AIDY := '  0.00';
                     ELSIF (AIDY_Award_Count = File_Read_Count) THEN
                        pct_TERM := '  0.00';
                        pct_AIDY := '100.00';
                     ELSIF ((TERM_Award_Count > 0) OR (AIDY_Award_Count > 0)) THEN
                        pct_TERM := Trim(To_Char((100 * (TERM_Award_Count / Param_Data_ROWS_with_Award)), '990.00'));
                        pct_AIDY := Trim(To_Char((100 * (AIDY_Award_Count / Param_Data_ROWS_with_Award)), '990.00'));
                        SELECT Max( len ) INTO Digit_Count
                        FROM
                           (
                           SELECT Length(To_Char(Trunc(pct_TERM))) len FROM dual
                        UNION
                           SELECT Length(To_Char(Trunc(pct_AIDY))) len FROM dual
                           );
                        pct_TERM := LPad( pct_TERM, Digit_Count+3 );
                        pct_AIDY := LPad( pct_AIDY, Digit_Count+3 );
                     ELSE
                        pct_TERM := '0.00';
                        pct_AIDY := '0.00';
                     END IF;
                  END IF;
                  SELECT Length(To_Char(Trunc(File_Read_Count))) INTO Digit_Count FROM dual;
                  Dbms_Output.Put_Line( ' ...All '||File_Read_Count||' INPUT data rows are in the correct format; with [Digit_Count='||Digit_Count||']');
                  Dbms_Output.Put_Line( '    '||  LPad(To_Char(Total_Param_Data_ROWS),        Digit_Count)||' total Input Parameter Rows');
                  Dbms_Output.Put_Line( '      '||LPad(To_Char(Total_Comment_ROWS),           Digit_Count)||' total Input Parameter Rows for Comments ONLY');
                  Dbms_Output.Put_Line( '      '||LPad(To_Char(Param_Data_ROWS_with_Award),   Digit_Count)||' total Input Parameter Rows for Fund Awarding');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(TERM_Award_Count), Digit_Count)||' formatted for TERM awards ['||pct_TERM||Pcnt||'], and with');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(AIDY_Award_Count), Digit_Count)||' formatted for AIDY awards ['||pct_AIDY||Pcnt||']'          );
                  Dbms_Output.Put_Line( '      '||LPad(To_Char(PCKG_LOAD_IND     ), Digit_Count)||' total PCKG Load Indicator(s) in file'   );
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(PCKG_LOAD_OK      ), Digit_Count)||' total OK values [1..5]');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(PCKG_LOAD_NULLs   ), Digit_Count)||' total NULL values; NULLs default to 1 (Full-Time)');
                  Dbms_Output.Put_Line( '      '||LPad(To_Char(AWRD_LTR_IND_Flags), Digit_Count)||' total AWRD LTR Indicator(s) in file'    );
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(AWRD_LTR_IND_Yes  ), Digit_Count)||' total "Y" values');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(AWRD_LTR_IND_No   ), Digit_Count)||' total "N" values');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(AWRD_LTR_IND_NULLs), Digit_Count)||' total NULL values; NULLs default to "Y"');
                  Dbms_Output.Put_Line( '      '||LPad(To_Char(RRRAREQ_Load_OK+RRRAREQ_Load_NoGo), Digit_Count)||' total RRRAREQ data row(s) in file'    );
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(RRRAREQ_Load_OK  ), Digit_Count)||' total with OK values');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(RRRAREQ_Load_NoGo), Digit_Count)||' total with NoGo values');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(RRRAREQ_Load_NULLs), Digit_Count)||' total NULL values; so no RRRAREQ action');
                  Dbms_Output.Put_Line( '      '||LPad(To_Char(RHACOMM_Data), Digit_Count)||' total Input Parameter Rows for RHACOMM');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(RHACOMM_No_Award  ), Digit_Count)||' for Comments Only');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(RHACOMM_with_Award), Digit_Count)||' with Fund Awarding');
                  Dbms_Output.Put_Line( '          '||LPad(To_Char(RHACOMM_Cat_Codes), Digit_Count)||' total Input Parameter Rows for RHACOMM CAT Code');
                  Dbms_Output.Put_Line( '      '||LPad(To_Char(RORMESG_Data), Digit_Count)||' total Input Parameter Rows for RORMESG Comments with a CAT Code');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(RORMESG_No_Award  ), Digit_Count)||' for Comments Only');
                  Dbms_Output.Put_Line( '        '||LPad(To_Char(RORMESG_with_Award), Digit_Count)||' with Fund Awarding');

                  G_Message_Body := G_Message_Body||CrLf||' ...All '||File_Read_Count||' INPUT data rows are in the correct format; with';
                  G_Message_Body := G_Message_Body||CrLf||'        '||LPad(To_Char(Sp_Fund_Count   ), Digit_Count)||' SFAO non-Special-Rule Restricted awards';
                  FOR s IN 1..Sp_Fund_Count LOOP
                     G_Message_Body := G_Message_Body||CrLf||'        .......... '||Special_Fund(s);
                  END LOOP;
                  G_Message_Body := G_Message_Body||CrLf||'    '||  LPad(To_Char(Total_Param_Data_ROWS),      Digit_Count)||' total Input Parameter Rows';
                  G_Message_Body := G_Message_Body||CrLf||'      '||LPad(To_Char(Total_Comment_ROWS),         Digit_Count)||' total Input Parameter Rows for Comments ONLY';
                  G_Message_Body := G_Message_Body||CrLf||'      '||LPad(To_Char(Param_Data_ROWS_with_Award), Digit_Count)||' total Input Parameter Rows for Fund Awarding';
                  G_Message_Body := G_Message_Body||CrLf||'        '||LPad(To_Char(TERM_Award_Count), Digit_Count)||' formatted for TERM awards ['||pct_TERM||Pcnt||']';
                  G_Message_Body := G_Message_Body||CrLf||'        '||LPad(To_Char(AIDY_Award_Count), Digit_Count)||' formatted for AIDY awards ['||pct_AIDY||Pcnt||']';
                  G_Message_Body := G_Message_Body||CrLf||'      '||LPad(To_Char(PCKG_LOAD_IND     ), Digit_Count)||' total PCKG Load Indicator(s) in file';
                  G_Message_Body := G_Message_Body||CrLf||'        '||LPad(To_Char(PCKG_LOAD_OK      ), Digit_Count)||' total OK values [1..5]';
                  G_Message_Body := G_Message_Body||CrLf||'        '||LPad(To_Char(PCKG_LOAD_NULLs   ), Digit_Count)||' total NULL values; NULLs default to 1 (Full-Time)';
                  G_Message_Body := G_Message_Body||CrLf||'      '||LPad(To_Char(AWRD_LTR_IND_Flags), Digit_Count)||' total AWRD LTR Indicator(s) in file';
                  G_Message_Body := G_Message_Body||CrLf||'        '||LPad(To_Char(AWRD_LTR_IND_Yes  ), Digit_Count)||' total "Y" values';
                  G_Message_Body := G_Message_Body||CrLf||'        '||LPad(To_Char(AWRD_LTR_IND_No   ), Digit_Count)||' total "N" values';
                  G_Message_Body := G_Message_Body||CrLf||'        '||LPad(To_Char(AWRD_LTR_IND_NULLs), Digit_Count)||' total NULL values; NULLs default to "Y"';
                  G_Message_Body := G_Message_Body||CrLf||'      '||LPad(To_Char(RRRAREQ_Load_OK+RRRAREQ_Load_NoGo), Digit_Count)||' total RRRAREQ data row(s) in file';
                  G_Message_Body := G_Message_Body||CrLf||'        '||LPad(To_Char(RRRAREQ_Load_OK  ), Digit_Count)||' total with OK values';
                  G_Message_Body := G_Message_Body||CrLf||'        '||LPad(To_Char(RRRAREQ_Load_NoGo), Digit_Count)||' total with NoGo values';
                  G_Message_Body := G_Message_Body||CrLf||'        '||LPad(To_Char(RRRAREQ_Load_NULLs), Digit_Count)||' total NULL values; so no RRRAREQ action';
                  G_Message_Body := G_Message_Body||CrLf||'      '||LPad(To_Char(RHACOMM_Data), Digit_Count)||' total Input Parameter Rows for RHACOMM';
                  G_Message_Body := G_Message_Body||CrLf||'        '||LPad(To_Char(RHACOMM_No_Award  ), Digit_Count)||' for Comments Only';
                  G_Message_Body := G_Message_Body||CrLf||'        '||LPad(To_Char(RHACOMM_with_Award), Digit_Count)||' with Fund Awarding';
                  G_Message_Body := G_Message_Body||CrLf||'          '||LPad(To_Char(RHACOMM_Cat_Codes), Digit_Count)||' total Input Parameter Rows for RHACOMM CAT Code';
                  G_Message_Body := G_Message_Body||CrLf||'      '||LPad(To_Char(RORMESG_Data), Digit_Count)||' total Input Parameter Rows for ROAMESG Comments';
                  G_Message_Body := G_Message_Body||CrLf||'        '||LPad(To_Char(RORMESG_No_Award  ), Digit_Count)||' for Comments Only';
                  G_Message_Body := G_Message_Body||CrLf||'        '||LPad(To_Char(RORMESG_with_Award), Digit_Count)||' with Fund Awarding';
               END IF;

               Utl_File.FCLOSE( G_INPUT_handle );
               IF ((Param_Data_ROWS_with_Award > 0) AND (ROW_Count != 0)) THEN
                  Dbms_Output.Put_Line( '================================================================' );
                  Dbms_Output.Put_Line( '*******  RZRSCIN is NOT empty  *******' );
                  Dbms_Output.Put_Line( 'Your awarding request cannot be processed now.  Try again later.' );
                  Dbms_Output.Put_Line( '================================================================' );
                  G_Message_Body := G_Message_Body||CrLf||'================================================================';
                  G_Message_Body := G_Message_Body||CrLf||'*******  RZRSCIN is NOT empty  *******';
                  G_Message_Body := G_Message_Body||CrLf||'Your awarding request cannot be processed now.  Try again later.';
                  G_Message_Body := G_Message_Body||CrLf||'================================================================';
                  RZRSCIN_ind := 999;
               END IF;
               IF OK_to_Process_Input_Data THEN
                  G_HISTORY_filename := G_BASE_filename||'_'||To_Char(File_Read_Count)||'_'||c_rpt_date_str||'_'||One_Up_Number||G_file_extention;
                  Dbms_Output.Put_Line( '   File_Directory: '||  G_file_directory );
                  Dbms_Output.Put_Line( '  Input_File_Name: '||  G_INPUT_filename );
                  IF (ROW_Count = 0) THEN Dbms_Output.Put_Line( 'Archive_File_Name: '||G_HISTORY_filename ); END IF;
                  Dbms_Output.Put_Line( '      exists_flag: '||            'TRUE' );
                  Dbms_Output.Put_Line( '~~~  ~~~  ~~~  ~~~  ~~~  ~~~  ~~~  ~~~'  );
                  Dbms_Output.Put_Line( '  Input Data Rows: '||File_Read_Count    );
                  Dbms_Output.Put_Line( '======================================'||CrLf  );

                  G_Message_Body := G_Message_Body||CrLf||'   File_Directory: '||  G_file_directory;
                  G_Message_Body := G_Message_Body||CrLf||'  Input_File_Name: '||  G_INPUT_filename ;
                  IF (ROW_Count = 0) THEN G_Message_Body := G_Message_Body||CrLf||'Archive_File_Name: '||G_HISTORY_filename; END IF;
                  G_Message_Body := G_Message_Body||CrLf||'      exists_flag: '||            'TRUE';
                  G_Message_Body := G_Message_Body||CrLf||'=======================================================';
                  G_Message_Body := G_Message_Body||CrLf||'  Input Data Rows: '||File_Read_Count;
                  G_Message_Body := G_Message_Body||CrLf||'======================================================='||CrLf;

                  G_INPUT_handle := Utl_File.fopen(G_file_directory,         G_INPUT_filename,   'R');

                  File_Read_ERRORS    := 0;
                  File_Read_Count     := 0;
                  File_Data_Length    := 0;
                  Unusable_Data_Count := 0;

                  LOOP
                     <<Data_Process_Block>>
                     BEGIN
                        Utl_File.GET_LINE(G_INPUT_handle, Download_Data);
                        Download_Data := RTrim_CTRL( Download_Data );
/*
                        n := Length(Download_Data);
                        IF (n > 1) THEN
                           IF (ASCII(SubStr(Download_Data, 1, n)) < 32) THEN
                              Download_Data := SubStr(Download_Data, 1, n-1);
                           END IF;
                        END IF;
*/
                        File_Read_Count  := File_Read_Count + 1;
                        File_Data_Length := Length(Download_Data);
                        tmpNumber := Nvl(Length(REPLACE(Download_Data, Chr(9), '')), 0);

                        IF ((File_Read_Count > Sp_Fund_Count) AND (tmpNumber > 0))THEN
                           <<Data_Length_Check>>
                           BEGIN
                              IF (( File_Data_Length > 0 ) AND ( File_Data_Length < 4000  )) THEN
                                 -- Initialize the data fields...
                                 G_UNM_BanID         := NULL;
                                 G_AWARD_AIDY        := NULL;
                                 G_AWARD_TERM        := NULL;
                                 G_FUND_Code         := NULL;
                                 G_AWARD_Amount      := NULL;
                                 G_AWARD_Action      := NULL;
                                 G_AWRD_LTR_IND      := NULL;
                                 G_Academic_Load     := NULL;
                                 G_RHRCOMM_Cat_Code  := NULL;
                                 G_Text_for_RHRCOMM  := NULL;
                                 G_EXDY_for_RORMESG  := NULL;
                                 G_CODE_for_RORMESG  := NULL;
                                 G_Text_for_RORMESG  := NULL;
                                 G_RRRAREQ_TREQ_CODE := NULL;
                                 G_RRRAREQ_TRST_CODE := NULL;
                                 G_RRRAREQ_PERIOD    := NULL;
                                 --====================================================================
                                 -- Parse the data string into individual fields...
                                 L_fld := unm.ArrayTables.f_Parse_String( Download_Data, HT, Field_Count );
-- -- -- FOR h IN 1..Field_Count LOOP
-- -- --    Dbms_Output.Put_Line( '~~~~~['||Field_Count||']~~~~~ L_fld('||To_Char(h, '90')||') ==>'||SubStr(L_fld(h), 1, 35)||'<==' );
-- -- -- END LOOP;
                                 G_AWARD_AIDY   :=           L_fld(1);
                                 G_UNM_BanID    :=           L_fld(2);
                                 G_FUND_Code    :=           L_fld(3);
                                 G_AWARD_TERM   :=           L_fld(4);
                                 G_AWARD_Amount := To_Number(L_fld(5));
                                 G_AWARD_Action :=    SubStr(L_fld(6), 1, 4);
                                 /*==================================================================*/
                                 IF (Field_Count >=  7) THEN G_AWRD_LTR_IND      := Upper(L_fld(7)); END IF;
                                 IF (Field_Count >=  8) THEN G_Academic_Load     := L_fld( 8);       END IF;
                                 IF (Field_Count >=  9) THEN G_RRRAREQ_TREQ_CODE := L_fld( 9);       END IF;
                                 IF (Field_Count >= 10) THEN G_RRRAREQ_TRST_CODE := L_fld(10);       END IF;
                                 IF (Field_Count >= 11) THEN G_RRRAREQ_PERIOD    := L_fld(11);       END IF;
                                 IF (Field_Count >= 12) THEN G_RHRCOMM_Cat_Code  := L_fld(12);       END IF;
                                 IF (Field_Count >= 13) THEN G_Text_for_RHRCOMM  := L_fld(13);       END IF;
                                 IF (Field_Count >= 14) THEN Proc_RORMESG_Date_CK(  L_fld(14), DATA_is_OK, G_EXDY_for_RORMESG ); END IF;
                                 IF (Field_Count >= 15) THEN G_CODE_for_RORMESG  := L_fld(15);       END IF;
                                 IF (Field_Count >= 16) THEN G_Text_for_RORMESG  := L_fld(16);       END IF;

                                 tmpString2 := Upper(Nvl(L_fld(3), 'N/A'));
                                 IF (tmpString2 = 'COMMENTONLY') THEN
                                    G_Banner_PIDM   := f_get_pidm( G_UNM_BanID );
                                    Some_Data := FALSE;
                                    indx := 12;
                                    IF (Field_Count >= indx) THEN
                                       IF ((Nvl(L_fld(indx), 'x')||Nvl(L_fld(indx+1), 'x')) != 'xx') THEN
                                          Some_Data := TRUE;
                                          LOAD_Check := unm.rzkflib.f_Insert_RHRCOMM_Record( G_Banner_PIDM, G_AWARD_AIDY, G_Text_for_RHRCOMM, G_RHRCOMM_Cat_Code );
                                          IF LOAD_Check THEN RHRCOMM_Load_OK := RHRCOMM_Load_OK + 1;
                                          ELSE RHRCOMM_Load_FAILED := RHRCOMM_Load_FAILED + 1; END IF;
                                       END IF;
                                    END IF;
                                    indx := 14;
                                    IF (Field_Count >= indx) THEN
                                       IF ((Nvl(L_fld(indx), 'y')||Nvl(L_fld(indx+1), 'y')||Nvl(L_fld(indx+2), 'y')) != 'yyy') THEN
                                          Some_Data := TRUE;
                                          LOAD_Check := unm.rzkflib.f_Insert_RORMESG_Record( G_Banner_PIDM, G_AWARD_AIDY, G_EXDY_for_RORMESG, G_CODE_for_RORMESG, G_Text_for_RORMESG );
                                          IF LOAD_Check THEN RORMESG_Load_OK := RORMESG_Load_OK + 1;
                                          ELSE RORMESG_Load_FAILED := RORMESG_Load_FAILED + 1; END IF;
                                       END IF;
                                    END IF;
                                    IF Some_Data THEN
                                       Some_Comment_ONLY_Data := Some_Comment_ONLY_Data + 1;
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
                                                           RZRSCIN_RHRCOMM_Cat,
                                                           RZRSCIN_RHRCOMM_Txt,
                                                           RZRSCIN_RORMESG_EXPR,
                                                           RZRSCIN_RORMESG_CODE,
                                                           RZRSCIN_RORMESG_Txt
                                                          )
                                       VALUES(G_UNM_BanID   ,
                                              G_AWARD_AIDY  ,
                                              G_AWARD_TERM  ,
                                              G_FUND_Code   ,
                                              G_AWARD_Amount,
                                              G_AWARD_Action,
                                              G_AWRD_LTR_IND,
                                              G_Academic_Load,
                                              G_RRRAREQ_TREQ_CODE,
                                              G_RRRAREQ_TRST_CODE,
                                              G_RRRAREQ_PERIOD,
                                              G_RHRCOMM_Cat_Code,
                                              G_Text_for_RHRCOMM,
                                              G_EXDY_for_RORMESG,
                                              G_CODE_for_RORMESG,
                                              G_Text_for_RORMESG
                                             );

                                       COMMIT;
                                       --ROLLBACK;
                                       Table_Insert_Count := Table_Insert_Count + 1;
                                    EXCEPTION
                                       WHEN OTHERS THEN
                                          Dbms_Output.Put_Line( '['||File_Read_Count||'] Exception INSERTING Data, File_Data_Length== '||File_Data_Length );
                                          unm.rzkflib.Track_Errors;
                                    END;
                                 END IF;
                              ELSE
                                 Dbms_Output.Put_Line( '['||File_Read_Count||'] File Data is not usable, the Data read is '||File_Data_Length||' characters long' );
                                 Unusable_Data_Count := Unusable_Data_Count + 0;
                              END IF;  -- end of: IF (( File_Data_Length > 0 ) ...) THEN

                           EXCEPTION
                              WHEN OTHERS THEN
                                 Dbms_Output.Put_Line( '['||File_Read_Count||'] Exception in Data_Length_Check -- ' );
                                 unm.rzkflib.Track_Errors;
                           END Data_Length_Check;
                        END IF;

                     EXCEPTION
                        WHEN NO_DATA_FOUND THEN
                           EXIT;
                        WHEN OTHERS THEN
                           IF SQLCODE != -29284 THEN
                              Dbms_Output.Put_Line( '['||File_Read_Count||'] Exception in Data_Process_Block -- ' );
                              unm.rzkflib.Track_Errors;
                           END IF;
                           File_Read_ERRORS := File_Read_ERRORS + 1;
                     END Data_Process_Block;
                  END LOOP;
IF ((Instance_Name = 'DEVL') AND (User_Name != 'DASMITH')) THEN
                  Utl_File.FCOPY( g_file_directory, G_INPUT_filename, g_file_history_directory, G_HISTORY_filename );
                  Utl_File.fclose( G_INPUT_handle   );
                  Utl_File.fremove( g_file_directory, G_INPUT_filename );
END IF;
               ELSE
                  Utl_File.fclose( G_INPUT_handle   );
               END IF;   -- end of: IF ((Total_Field_ERRORS = 0) AND (ROW_Count = 0)) THEN

               Utl_File.fclose( G_INPUT_handle   );

            END IF;  -- end of: IF Data_Import_File_Exists THEN

            IF (Table_Insert_Count = 0) THEN
               RZRSCIN_ind := 999;
            ELSE
               RZRSCIN_ind := 0;
            END IF;

            IF Data_Import_File_Exists THEN
--                RZRSCIN_ind := 0;
               Dbms_Output.Put_Line( 'Data_Import_File_Exists: TRUE  ==>  RZRSCIN_ind: '||RZRSCIN_ind );
            ELSE
--                RZRSCIN_ind := 999;
               Dbms_Output.Put_Line( 'Data_Import_File_Exists: FALSE  ==>  RZRSCIN_ind: '||RZRSCIN_ind );
            END IF;

            SELECT Count(*) INTO cnt FROM RZRSCIN;

            --=============================================================================
            -- Send an email to the user with the summary data...
            --  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~  ~~~~~
            IF (Some_Comment_ONLY_Data > 0) THEN
               IF (Total_Comment_ROWS = 1) THEN
                  G_Message_Body := G_Message_Body||CrLf||CrLf||'There was 1 row of data for COMMENTONLY'||CrLf;
               ELSE
                  G_Message_Body := G_Message_Body||CrLf||CrLf||'There were '||Total_Comment_ROWS||' rows of data for COMMENTONLY'||CrLf;
               END IF;
               xx := Total_Comment_ROWS - Some_Comment_ONLY_Data;
               IF ( xx = 1 ) THEN
                  G_Message_Body := G_Message_Body||'     1 row for COMMENTONLY with NO DATA'||CrLf;
               ELSIF (xx > 1) THEN
                  G_Message_Body := G_Message_Body||'     '||xx||' rows for COMMENTONLY with NO DATA'||CrLf;
               END IF;
               xx := RHRCOMM_Load_OK + RHRCOMM_Load_FAILED;
               IF (xx = 1) THEN
                  G_Message_Body := G_Message_Body||'     1 row of data for RHACOMM: ';
               ELSE
                  G_Message_Body := G_Message_Body||'     '||xx||' rows of data for RHACOMM: ';
               END IF;
               G_Message_Body := G_Message_Body||RHRCOMM_Load_OK||' loaded OK and '||RHRCOMM_Load_FAILED||' FAILED'||CrLf;
               xx := RORMESG_Load_OK + RORMESG_Load_FAILED;
               IF (xx = 1) THEN
                  G_Message_Body := G_Message_Body||'     1 row of data for ROAMESG: ';
               ELSE
                  G_Message_Body := G_Message_Body||'     '||xx||' rows of data for ROAMESG: ';
               END IF;
               G_Message_Body := G_Message_Body||RORMESG_Load_OK||' loaded OK and '||RORMESG_Load_FAILED||' FAILED'||CrLf;
            END IF;

            IF (RZRSCIN_ind > 0) THEN
               G_Message_Body := G_Message_Body||CrLf||'No data was loaded into RZRSCIN'||CrLf;
            ELSE
               G_Message_Body := G_Message_Body||CrLf||Table_Insert_Count||' rows of data were loaded into RZRSCIN'||CrLf;
            END IF;

            IF (cnt = 1) THEN
               IF (cnt = ROW_Count) THEN
                  G_Message_Body := G_Message_Body||CrLf||'There are the same number of rows of data [' || cnt || '] in RZRSCIN'||CrLf;
               ELSE
                  G_Message_Body := G_Message_Body||CrLf||'There is now 1 row of data in RZRSCIN'||CrLf;
               END IF;
            ELSE
               IF (cnt = ROW_Count) THEN
                  G_Message_Body := G_Message_Body||CrLf||'There are the same number of rows of data [' || cnt || '] in RZRSCIN'||CrLf;
               ELSE
                  G_Message_Body := G_Message_Body||CrLf||'There are now ' || cnt || ' rows of data in RZRSCIN'||CrLf;
               END IF;
            END IF;
         END IF;  -- end of: IF (User_Name != 'FINAIDAPPWORX') THEN
      END READ_Data_Block;

      rzkflib.p_user_email( 'RZFLSCP_'||Instance_Name, 'Load Scholarship Parameters [dB: '||Instance_Name||']',
                            G_Message_Body, 'FINAID-TECH-L@unm.edu' );
   END User_Name_OK_Block;
   --=============================================================================

   Dbms_Output.new_line;

   --========================================================================
   -- Search the Dbms_Output buffer for the text "ERROR" and replace all
   -- instances with the text "ISSUE"
   --------------------------------------------------------------------------
   p_DBMS_Text_Replace( 'ERROR', 'ISSUE' );
   p_DBMS_Text_Replace( 'error', 'ISSUE' );
   p_DBMS_Text_Replace( 'ORA-',  'Oracle-' );

   --========================================================================
   -- This function returns the value of "Function_Status" which is equal to
   -- the sum of all data issues detected, including an indicator value for
   -- loading the data into the SCholarship INput table, RZRSCIN.
   --------------------------------------------------------------------------
   Function_Status := Unusable_Data_Count + File_Read_ERRORS + Total_Field_ERRORS + RZRSCIN_ind + Unathorized_User;
   IF (User_Name != 'FINAIDAPPWORX') THEN
      Dbms_Output.Put_Line( 'Function_Status: '||Function_Status||
            ' == Unusable_Data_Count + File_Read_ERRORS + Total_Field_ERRORS + RZRSCIN_ind + Unathorized_User' );
      Dbms_Output.Put_Line( '                   == '||Unusable_Data_Count||' + '||
            File_Read_ERRORS||' + '||Total_Field_ERRORS||' + '||RZRSCIN_ind||' + '||Unathorized_User );
   END IF;

--    Function_Status := 99;
--    Dbms_Output.Put_Line( '...reset for testing ==> Function_Status: '||Function_Status );

   RETURN Function_Status;

   EXCEPTION
      WHEN OTHERS THEN
         unm.rzkflib.Track_Errors;
      RETURN -5;

END RZFLSCP;
/

CREATE OR REPLACE PUBLIC SYNONYM RZFLSCP FOR UNM.RZFLSCP;
