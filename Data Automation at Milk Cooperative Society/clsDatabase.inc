'-----------------------
'clsDatabase.inc
'Class using SQlite
'-------------------------
#INCLUDE ONCE  "sqlite3.inc"
'#INCLUDE once  "sqliteobject.inc"

'=====================================================================
'CLASS clsDatabase
'=====================================================================
CLASS clsDatabase
INSTANCE sDatabaseFilename AS STRING
INSTANCE dVersionNumber AS SINGLE
INSTANCE oConn AS ISQLite3Connection
INSTANCE oCmd  AS ISQLite3Command
INSTANCE lOpen AS LONG
INSTANCE sOldKey AS STRING              ' used in reading of records
INSTANCE oReader AS ISQLite3Reader
'LOCAL r     AS ISQLite3Reader

'-------------------------------------------------------
CLASS METHOD DESTROY()
END METHOD

'-------------------------------------------------------
CLASS METHOD CREATE()
sDatabaseFilename=""
END METHOD

'-------------------------------------------------------
INTERFACE DatabaseInterface
INHERIT IUNKNOWN

'-------------------------------------------------------
'PROPERTY GET LastError()
' returns 0 if no error was encountered during last data base table access
' returns a number>0 if error
'-------------------------------------------------------
PROPERTY GET LastError() AS LONG
IF oCmd.LastError <> %SQLITE_DONE THEN
    PROPERTY=1      ' error occured
ELSE
    PROPERTY=0      ' no error
END IF

END PROPERTY
'-------------------------------------------------------
'METHOD Init(sName AS STRING)
' sets the database file name
'-------------------------------------------------------
METHOD Init(sName AS STRING)
sDatabaseFilename=sName
END METHOD
'-------------------------------------------------------
'Method OpenDatabase
'opens database file name
'-------------------------------------------------------
METHOD OpenDatabase()
IF LEN(sDatabaseFilename)=0 THEN EXIT METHOD

' Instantiate a connection class.
oConn = CLASS "clsSQLite3Connection"

' Open the database, it will create a new one if not exist.
IF oConn.Open(sDatabaseFilename) THEN
    ' Instantiate a command object.
    oCmd = CLASS "clsSQLite3Command"
    ' Set a reference to the connection object.
    oCmd.Connection = oConn
    lOpen=1
    oReader=NOTHING
ELSE
    lOpen=0
END IF

END METHOD
'-------------------------------------------------------
'Method CloseDatabase
' closes connection to databse
'-------------------------------------------------------
METHOD CloseDatabase
' Release the object and connection.
oCmd = NOTHING
oConn = NOTHING
lOpen=0
END METHOD
'-------------------------------------------------------
'METHOD RebuildDatabase() AS LONG
'Rebuilds and cleansupp all unused space in the database
'-------------------------------------------------------
METHOD RebuildDatabase() AS LONG

LOCAL hr    AS LONG

IF lOpen=0 THEN
    METHOD=0
    EXIT METHOD
END IF

' Set the SQL statement.
oCmd.SQL = "VACUUM;"

' Execute the statement, use the lasterror to determine an error.
' hr will return 0
hr = oCmd.Execute()
' Was there an error?
IF oCmd.LastError <> %SQLITE_DONE THEN
    METHOD=0            ' error
ELSE
    METHOD=1            ' success
END IF
END METHOD
'-------------------------------------------------------
'METHOD CreateTable(sTableName AS STRING) AS LONG
'creates a table if it does not exist already
'input: table name string
'returns : 1 if table is created succesfully
'        :  0 if some error encountered while creating
'-------------------------------------------------------
METHOD CreateTable(sTableName AS STRING) AS LONG

LOCAL hr    AS LONG

IF lOpen=0 THEN
    METHOD=0
    EXIT METHOD
END IF

' Set the SQL statement.
'oCmd.SQL = "CREATE TABLE IF NOT EXISTS ["+sTableName+"] ( [RECORDID] INTEGER PRIMARY KEY AUTO_INCREMENT , [RECORDKEY] TEXT UNIQUE , [RECORDDATA] BLOB );"
oCmd.SQL = "CREATE TABLE IF NOT EXISTS "+sTableName+" ( RECORDID INTEGER PRIMARY KEY  , RECORDKEY TEXT  , RECORDDATA BLOB );"
hr = oCmd.Execute()
IF oCmd.LastError <> %SQLITE_DONE THEN
    METHOD=0            ' error
ELSE
    METHOD=1            ' success
END IF
END METHOD
'-------------------------------------------------------
'METHOD DeleteTable(sTableName AS STRING) AS LONG
'delete teh specified table
'input: table name string
'returns : 1 if table is deleted succesfully
'        :  0 if some error encountered while deleteing
'-------------------------------------------------------
METHOD DeleteTable(sTableName AS STRING) AS LONG

LOCAL hr    AS LONG

IF lOpen=0 THEN
    METHOD=0
    EXIT METHOD
END IF

' Set the SQL statement.
oCmd.SQL = "DROP TABLE ["+sTableName+"];"
' Execute the statement, use the lasterror to determine an error.
' hr will return 0
hr = oCmd.Execute()
' Was there an error?
IF oCmd.LastError <> %SQLITE_DONE THEN
    METHOD=0            ' error
ELSE
    METHOD=1            ' success
END IF
END METHOD
'-------------------------------------------------------
'METHOD AddRecord(sKeyValue AS STRING,sRecord AS STRING) AS LONG
'Adds a given record
'Each record cosnists of a Key whchis ia string variable and a BLOB.
'The BLOB contains a record structure as per application requirement
'input:sTableName is the table into which data is to be added
'          sKeyValue is teh keyvalue string and sRecod is a BLOB
'returns : 1 if record added succesfully
'        :  0 if some error encountered
'-------------------------------------------------------
METHOD AddRecord(sTableName AS STRING,sKeyValue AS STRING,sBlobdata AS STRING) AS LONG

LOCAL hr    AS LONG
LOCAL p1i   AS ISQLite3ParameterInt
LOCAL p2s   AS ISQLite3ParameterStr
LOCAL p3b   AS ISQLite3ParameterBlob


IF lOpen=0 THEN
    METHOD=0
    EXIT METHOD
END IF

' Set the SQL statement.
'oCmd.SQL = "INSERT INTO ["+sTableName+"] ( [RECORDKEY]  , [RECORDDATA] ) VALUES( @RECORDKEY,@RECORDDATA );"
oCmd.SQL = "INSERT INTO "+sTableName+" ( RECORDKEY  , RECORDDATA ) VALUES ( @RECORDKEY,@RECORDDATA );"
' On each new record we clear the command's parameters and use new instances.
' This may be time consuming but the test loop by itself is not common practise anyway.
oCmd.Parameters.Clear()
' Create a string parameter.
p2s = CLASS "clsSQLite3ParameterStr"
' Set the name for the value.
p2s.Name = "@RECORDKEY"
' Set the value.
p2s.Value = sKeyValue
' Add to the command.
oCmd.Parameters.Add( p2s )

' Create a blob parameter.
p3b = CLASS "clsSQLite3ParameterBlob"
p3b.Name = "@RECORDDATA"

' Blob binding is done via the address of the data and its size.
p3b.Value = STRPTR( sBlobdata )
p3b.Size = LEN( sBlobdata )
oCmd.Parameters.Add( p3b )
' Execute the statement, use the lasterror to determine an error.
' hr will return 0
hr = oCmd.Execute()
p1i=NOTHING
p2s=NOTHING
p3b=NOTHING
' Was there an error?
IF oCmd.LastError <> %SQLITE_DONE THEN
    METHOD=0            ' error
ELSE
    METHOD=1            ' success
END IF
END METHOD
'-------------------------------------------------------
'METHOD GetRecord(sTableName AS STRING,sKeyValue AS STRING,BYREF sRecord AS STRING) AS LONG
'Gets a pecified record
'Each record consists of a Key whchis ia string variable and a BLOB.
'The BLOB contains a record structure as per application requirement
'input:sTableName is the table into which data is to be added
'          sKeyValue is teh keyvalue string and sRecod is a BLOB
'returns : 1 if record read succesfully
'        :  0 if some error encountered
'-------------------------------------------------------
METHOD GetRecord(sTableName AS STRING,BYREF lRecordID AS LONG, BYVAL sKeyValue AS STRING,BYREF sBlobdata AS STRING) AS LONG

LOCAL hr    AS LONG
LOCAL p1i   AS ISQLite3ParameterInt
LOCAL p2s   AS ISQLite3ParameterStr
LOCAL p3b   AS ISQLite3ParameterBlob

LOCAL l1 AS LONG

LOCAL lResult AS LONG

lResult=0
IF lOpen=0 THEN
    METHOD=lResult
    EXIT METHOD
END IF

'----------------------
IF sOldkey<>sKeyValue THEN
    oReader=NOTHING
END IF

IF NOT ISOBJECT(oReader)   THEN
    l1=1
ELSE
    IF sOldkey=sKeyValue THEN
        l1=2
    ELSE
        l1=3
    END IF
END IF
IF sKeyValue="*" AND sOldkey<>"*" THEN l1=4
'----------------------

SELECT CASE l1
    CASE 1,3:             ' reader object ot present or it is a new key
        oCmd.SQL = "SELECT  RECORDID,RECORDKEY,RECORDDATA FROM "+sTableName+" WHERE RECORDKEY = @RECORDKEY;"
        p2s = CLASS "clsSQLite3ParameterStr"

        p1i = CLASS "clsSQLite3ParameterInt"
        p1i.Name = "@RECORDID"
        p1i.Value = lRecordID
        oCmd.Parameters.Add( p1i )

        p2s.Name = "@RECORDKEY"
        p2s.Value = sKeyValue
        sOldkey=sKeyValue
        oCmd.Parameters.Add( p2s )
        oReader=NOTHING             ' discard the old object pointer
        oReader = oCmd.ExecuteReader()     ' Thi reads all records that match the key
    CASE 2:             ' reader object present from previous read and also key is same as old. So it is a repeat read. Step it up
        oReader.Read()
    CASE 4:             ' reader object ot present or it is a new key
        oCmd.SQL = "SELECT ALL RECORDID, RECORDKEY , RECORDDATA FROM "+sTableName'+" WHERE RECORDKEY = @RECORDKEY;"
        oReader=NOTHING             ' discard the old object pointer
        sOldkey=sKeyValue
        oReader = oCmd.ExecuteReader()     ' Thi reads all records that match the key
END SELECT

IF (NOT ISOBJECT( oReader ))  OR (oReader.EOF()) THEN ' object not present or eof reached
    lResult=0
ELSE
    sBlobdata=oReader.Value(0)  ' Object present, value read
    lRecordID=VAL(oReader.Value(0))
    sBlobdata=oReader.Value(2)  ' Object present, value read
    sBlobdata=oReader.Value(3)  ' Object present, value read
    sBlobdata=oReader.Value(1)  ' Object present, value read
    lResult=1
END IF

p1i=NOTHING
p2s=NOTHING
p3b=NOTHING

METHOD=lResult
END METHOD

'-------------------------------------------------------
'METHOD UpdateRecord(sTableName AS STRING,sKeyValue AS STRING,BYREF sBlobdata AS STRING) AS LONG
'Gets a pecified record
'Each record consists of a Key whchis ia string variable and a BLOB.
'The BLOB contains a record structure as per application requirement
'input:sTableName is the table into which data is to be added
'          sKeyValue is teh keyvalue string and sRecod is a BLOB
'returns : 1 if record read succesfully
'        :  0 if some error encountered
'-------------------------------------------------------
METHOD UpdateRecord(sTableName AS STRING,BYVAL lRecordID AS LONG,sKeyValue AS STRING,BYREF sBlobdata AS STRING) AS LONG

LOCAL hr    AS LONG
LOCAL p1i   AS ISQLite3ParameterInt
LOCAL p2s   AS ISQLite3ParameterStr
LOCAL p3b   AS ISQLite3ParameterBlob

LOCAL l1 AS LONG

LOCAL lResult AS LONG

lResult=0
IF lOpen=0 THEN
    METHOD=lResult
    EXIT METHOD
END IF

oCmd.SQL = "UPDATE  "+sTableName+" SET RECORDDATA = @RECORDDATA WHERE RECORDID=@RECORDID;"
p1i = CLASS "clsSQLite3ParameterInt"
p1i.Name = "@RECORDID"
p1i.Value = lRecordID
oCmd.Parameters.Add( p1i )

'p2s = CLASS "clsSQLite3ParameterStr"
'p2s.Name = "@RECORDKEY"
'p2s.Value = sKeyValue
'oCmd.Parameters.Add( p2s )

p2s = CLASS "clsSQLite3ParameterStr"
p2s.Name = "@RECORDDATA"
p2s.Value = sBlobData
oCmd.Parameters.Add( p2s )

hr = oCmd.Execute()
p1i=NOTHING
p2s=NOTHING
p3b=NOTHING
' Was there an error?
IF oCmd.LastError <> %SQLITE_DONE THEN
    METHOD=0            ' error
ELSE
    METHOD=1            ' success
END IF

END METHOD

'-------------------------------------------------------

END INTERFACE
'-------------------------------------------------------
END CLASS
'-------------------------------------------------------
