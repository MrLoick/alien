# Alien Programming #
## Transforming Elements, Fast and Reliable, Complex Build and Utility ##
**Always Updating**

`*`
`Fund Driven`
`*`
```
bool AndroidAccessory::switchDevice(byte addr)
{
    int protocol = getProtocol(addr);
    if (protocol >= 1) {
        Serial.print("device supports protocol 1 or higher\n");
    } else {
        Serial.print("could not read device protocol version\n");
        return false;
    }

    sendString(addr, ACCESSORY_STRING_MANUFACTURER, manufacturer);
    sendString(addr, ACCESSORY_STRING_MODEL, model);
    sendString(addr, ACCESSORY_STRING_DESCRIPTION, description);
    sendString(addr, ACCESSORY_STRING_VERSION, version);
    sendString(addr, ACCESSORY_STRING_URI, uri);
    sendString(addr, ACCESSORY_STRING_SERIAL, serial);

    usb.ctrlReq(addr, 0, USB_SETUP_HOST_TO_DEVICE | USB_SETUP_TYPE_VENDOR |
                USB_SETUP_RECIPIENT_DEVICE, ACCESSORY_START, 0, 0, 0, 0, NULL);
    return true;
}

```


```
/* Control request for registering a HID device.
 * Upon registering, a unique ID is sent by the accessory in the
 * value parameter. This ID will be used for future commands for
 * the device
 *
 *  requestType:    USB_DIR_OUT | USB_TYPE_VENDOR
 *  request:        ACCESSORY_REGISTER_HID_DEVICE
 *  value:          Accessory assigned ID for the HID device
 *  index:          total length of the HID report descriptor
 *  data            none
 */
#define ACCESSORY_REGISTER_HID         54

/* Control request for unregistering a HID device.
 *
 *  requestType:    USB_DIR_OUT | USB_TYPE_VENDOR
 *  request:        ACCESSORY_REGISTER_HID
 *  value:          Accessory assigned ID for the HID device
 *  index:          0
 *  data            none
 */
#define ACCESSORY_UNREGISTER_HID         55

/* Control request for sending the HID report descriptor.
 * If the HID descriptor is longer than the endpoint zero max packet size,
 * the descriptor will be sent in multiple ACCESSORY_SET_HID_REPORT_DESC
 * commands. The data for the descriptor must be sent sequentially
 * if multiple packets are needed.
 *
 *  requestType:    USB_DIR_OUT | USB_TYPE_VENDOR
 *  request:        ACCESSORY_SET_HID_REPORT_DESC
 *  value:          Accessory assigned ID for the HID device
 *  index:          offset of data in descriptor
 *                      (needed when HID descriptor is too big for one packet)
 *  data            the HID report descriptor
 */
#define ACCESSORY_SET_HID_REPORT_DESC         56

/* Control request for sending HID events.
 *
 *  requestType:    USB_DIR_OUT | USB_TYPE_VENDOR
 *  request:        ACCESSORY_SEND_HID_EVENT
 *  value:          Accessory assigned ID for the HID device
 *  index:          0
 *  data            the HID report for the event
 */
#define ACCESSORY_SEND_HID_EVENT         57
```
```
**SET_AUDIO_MODE**
requestType:    USB_DIR_OUT | USB_TYPE_VENDOR
request:        58
value:          0 for no audio (default),
                1 for 2 channel, 16-bit PCM at 44100 KHz
index:          0
data            none

```


```
import pywapi
import pprint
pp = pprint.PrettyPrinter(indent=4)

countries = pywapi.get_countries_from_google('en')

pp.pprint(countries)
```


```
Public Sub AttachToProcesses(ByVal process_name As String, ByVal AttachIfCmdLineContains As String)
        ' Attaches to a process by its name. If a command line argument is needed, looks for it.'
        Dim pids As New System.Collections.Generic.List(Of Integer)
        Dim pids_debugged As New System.Collections.Generic.List(Of Integer)
        For Each debugged As EnvDTE.Process In DTE.Debugger.DebuggedProcesses
                pids_debugged.Add(debugged.ProcessID)
        Next
        Dim processes As System.Diagnostics.Process() = System.Diagnostics.Process.GetProcessesByName(process_name)
        For Each proc As System.Diagnostics.Process In processes
                If proc.MainModule.FileName().ToLower().Contains(AttachIfCmdLineContains.ToLower()) Then
                        pids.Add(proc.Id)
                End If
        Next
        For Each proc As EnvDTE.Process In DTE.Debugger.LocalProcesses
                If Not pids_debugged.Contains(proc.ProcessID) And pids.Contains(proc.ProcessID) Then
                        proc.Attach()
                End If
        Next
End Sub

Sub AttachChromium()
        ' Attaches to the chrome.exe processes that has \code\ in their command line'
        ' argument or binary path.'
        AttachToProcesses("chrome", System.IO.Path.GetDirectoryName(System.IO.Path.GetDirectoryName(DTE.Solution.FullName)) & "\build\")
End Sub
```


```
Sub FormatComment()
        ' Make comments fit 80 cols.'
        Dim sel As TextSelection = CType(DTE.ActiveDocument.Selection, TextSelection)
        Dim text As String = sel.Text
        Dim CommentMarker As String = "//"

        ' InStr() is one-based'
        Dim commentColumn As Integer = InStr(text, CommentMarker)
        ' Substring() is zero-based'
        Dim prefix As String = text.Substring(0, commentColumn - 1) + CommentMarker + " "
        ' Take in account the length of the comment marker and the space. The maximum is 81'
        ' and not 80; column starts at 1 and not 0. InStr() is 1 based too.'
        Dim maxline As Integer = 81 - commentColumn - CommentMarker.Length

        ' Remove comment marker'
        text = System.Text.RegularExpressions.Regex.Replace(text, "^ *// *", "", System.Text.RegularExpressions.RegexOptions.Multiline)
        ' Remove \r\n to put all the text on one line'
        text = System.Text.RegularExpressions.Regex.Replace(text, " *[" + vbLf + "]+ *", " ")
        text = text.Trim()

        Dim newtext As String = ""

        While text <> ""
                ' InStrRev() is one-based'
                Dim pos As Integer = InStrRev(text, " ", maxline)
                If pos = 0 Then
                        pos = text.Length
                End If

                ' Substring() is zero-based'
                Dim line As String = text.Substring(0, pos).Trim()
                newtext += prefix + line + vbLf
                text = text.Substring(pos)
        End While

        sel.Insert(newtext, vsInsertFlags.vsInsertFlagsContainNewText)
End Sub
```


```
Private Sub DocumentEvents_DocumentSaved(ByVal document As EnvDTE.Document) _
    Handles DocumentEvents.DocumentSaved
    Dim fileName As String
    Dim result As vsFindResult

    Try
        fileName = document.Name.ToLower()

        If fileName.EndsWith(".cc") _
        Or fileName.EndsWith(".cpp") _
        Or fileName.EndsWith(".c") _
        Or fileName.EndsWith(".h") Then
            ' Remove trailing whitespace'
            result = DTE.Find.FindReplace( _
                vsFindAction.vsFindActionReplaceAll, _
                "{:b}+$", _
                vsFindOptions.vsFindOptionsRegularExpression, _
                String.Empty, _
                vsFindTarget.vsFindTargetFiles, _
                document.FullName, _
                "", _
                vsFindResultsLocation.vsFindResultsNone)

            If result = vsFindResult.vsFindResultReplaced Then
                ' Triggers DocumentEvents_DocumentSaved event again'
                document.Save()
            End If
        End If
    Catch ex As Exception
        MsgBox(ex.Message, MsgBoxStyle.OkOnly, "Trim White Space exception")
    End Try
End Sub
```



```

Function TryOpenProjectItem(ByVal project_items As ProjectItems, ByVal item As String) As Boolean
        TryOpenProjectItem = False
        If project_items Is Nothing Then
                Exit Function
        End If
        For Each project_item As EnvDTE.ProjectItem In project_items
                If Strings.StrComp(project_item.Name, item, CompareMethod.Text) = 0 Then
                        ' Found!'
                        project_item.Open().Activate()
                        TryOpenProjectItem = True
                End If
                If project_item.SubProject Is Nothing Then
                        TryOpenProjectItem = TryOpenProjectItem(project_item.ProjectItems, item)
                Else
                        TryOpenProjectItem = TryOpenProjectItem(project_item.SubProject.ProjectItems, item)
                End If
                If TryOpenProjectItem = True Then
                        Exit Function
                End If
        Next
End Function

' Will find the file if it is:'
' - beside in the same directory,'
' - in "opened documents",'
' - in the same project'
' - TODO(maruel): Try in includes?'
Public Function TryOpen(ByVal FileName As String, ByVal Path As String, ByVal project As EnvDTE.Project) As Boolean
        TryOpen = False
        ' Try to open the file in same folder.'
        Try
                DTE.Documents.Open(Path + FileName, "Text")
                TryOpen = True
                Exit Function
        Catch
        End Try

        ' Search document in the same project.'
        If Not project Is Nothing Then
                TryOpen = TryOpenProjectItem(project.ProjectItems, FileName)
        End If

        ' Search opened documents.'
        For Each myDocument As EnvDTE.Document In DTE.Documents
                If Strings.StrComp(myDocument.Name, FileName, CompareMethod.Text) = 0 Then
                        Try
                                myDocument.Activate()
                                TryOpen = True
                                Exit Function
                        Catch
                        End Try
                End If
        Next
End Function

' Shortcut.'
Public Function TryOpen(ByVal FilePathName As String) As Boolean
        TryOpen = TryOpen(System.IO.Path.GetFileName(FilePathName), System.IO.Path.GetFullPath(FilePathName), Nothing)
End Function

' Will cycle thru .cc, .cpp, .h and .inl file extensions'
Public Sub SwitchOfSourceFile()
        ' For source looping. It\'s not extension in the true meaning.'
        Dim supportedExts As String() = {"-inl.h", ".cc", ".cpp", ".h", ".hpp", ".inl"}

        Dim origFile As String
        Dim origFilePath As String
        Dim project As EnvDTE.Project
        Try
                origFile = DTE.ActiveDocument.Name
                origFilePath = DTE.ActiveDocument.Path
                project = DTE.ActiveDocument.ProjectItem.ContainingProject
        Catch
                Exit Sub
        End Try

        ' This is touchy here because we want to support both ".h" and "-inl.h" so we have to find the right extension first.'
        For indexExt As Integer = 0 To supportedExts.Length - 1
                Dim ext As String = supportedExts(indexExt)
                If origFile.Length > ext.Length Then
                        If origFile.Substring(origFile.Length - ext.Length) = ext Then
                                Dim FileToOpen As String = origFile.Substring(0, origFile.Length - ext.Length)
                                ' Try the rest'
                                For indexTry As Integer = 0 To supportedExts.Length - 2
                                        Dim trueIndex As Integer = (indexExt + indexTry + 1) Mod supportedExts.Length
                                        If TryOpen(FileToOpen + supportedExts(trueIndex), origFilePath, project) Then
                                                ' We succeeded'
                                                Exit Sub
                                        End If
                                Next
                                ' We failed.'
                                Exit For
                        End If
                End If
        Next
        ' We failed.'
End Sub

```



```
Sub RunCurrentGTest()
        ' From the active source file, find the test that the user wants to run'
        ' based on the current cursor position. Set the project containing this'
        ' source file as the startup project, changes the command line to run'
        ' only this test, compile the project and starts it under the debugger.'
        ' Doesn\'t change any breakpoint.'
        Dim ActiveDoc As Document = DTE.ActiveDocument

        ' Try to guess the test to run.'
        Dim TestGroup As String = ""
        Dim TestName As String = ""
        Dim selection As TextSelection = CType(ActiveDoc.Selection(), TextSelection)
        Dim toppoint As EditPoint = selection.TopPoint.CreateEditPoint()
        Dim bottompoint As EditPoint = selection.BottomPoint.CreateEditPoint()
        Dim ranges As TextRanges = selection.TextRanges
        Dim line As Integer = selection.TopPoint.Line
        ' selection.FindPattern() is crummy.'
        While line <> 0
                selection.GotoLine(line)
                selection.SelectLine()
                Dim match As System.Text.RegularExpressions.Match = System.Text.RegularExpressions.Regex.Match(selection.Text, "TEST[_F]*\((.*),(.*)\)")
                If Not match Is System.Text.RegularExpressions.Match.Empty Then
                        TestGroup = match.Groups.Item(1).Value.Trim()
                        TestName = match.Groups.Item(2).Value.Trim()
                        Exit While
                End If
                line = line - 1
        End While
        ' Cheap way to try to restore the old selection. Isn\'t 100% correct.'
        selection.MoveToLineAndOffset(toppoint.Line, toppoint.LineCharOffset)
        selection.MoveToLineAndOffset(bottompoint.Line, bottompoint.LineCharOffset, True)

        ' From the current active document, find the project and the active configuration.'
        Dim Proj As Project = ActiveDoc.ProjectItem.ContainingProject
        Dim config As Configuration = Proj.ConfigurationManager.ActiveConfiguration

        ' Fix the command line argument.'
        Dim CmdLine As EnvDTE.Property = config.Properties.Item("CommandArguments")
        If TestGroup <> "" Then
                CmdLine.Value = "--gtest_filter=" & TestGroup & "." & TestName
        Else
                ' Run all'
                CmdLine.Value = ""
        End If

        ' Set it as startup project.'
        Dim SoluBuild As SolutionBuild = DTE.Solution.SolutionBuild
        Dim StartupProject As String
        StartupProject = Proj.UniqueName
        SoluBuild.StartupProjects = StartupProject

        ' Build it.'
        SoluBuild.BuildProject(config.ConfigurationName, Proj.UniqueName, True)

        ' Start it.'
        DTE.Debugger.Go()
End Sub


```


```
Public Sub AddDefinitionForSelectedDeclaration()
    ' Get the function declaration text.'
    Dim sel As TextSelection = DTE.ActiveDocument.Selection
    Dim text = sel.Text.Trim()

    Dim className = ClassNameFinder()

    Dim funcDef = text
    ' Remove comments first, since they mess up the rest of the regexes.'
    funcDef = Regex.Replace(funcDef, "//.*$", "", RegexOptions.Multiline)
    ' Try to put declarations all on the same line.'
    funcDef = Regex.Replace(funcDef, "([^;]) *\n *", "$1 ", RegexOptions.Singleline)
    ' Replace the identifier with ClassName::identifier.'
    funcDef = Regex.Replace(funcDef, "^(.*) ([^ ()]+;|[^ ()]+ *\()", _
                                "$1 " + className + "::$2", RegexOptions.Multiline)
    ' Convert ; to {} for functions.'
    funcDef = Regex.Replace(funcDef, "\) *;", ") {" + vbLf + "}" + vbLf)
    ' Remove leading whitespace, static/virtual.'
    funcDef = Regex.Replace(funcDef, "^ *", "", RegexOptions.Multiline)
    funcDef = Regex.Replace(funcDef, "static *", "// static" + vbLf)
    funcDef = Regex.Replace(funcDef, "virtual *", "")
    ' Collapse empty lines.'
    funcDef = Regex.Replace(funcDef, vbLf + vbLf + "+", vbLf + vbLf)

    ' Switch to source file and append defs at the end.'
    GoToCCFile()
    sel = DTE.ActiveDocument.Selection
    sel.EndOfDocument()
    sel.Insert(funcDef)
End Sub

' If the current document is an .h file, try to switch to the .cc file of the same name.'
Sub GoToCCFile()
    Dim origFile = DTE.ActiveDocument.FullName()
    If Regex.IsMatch(origFile, "\.h$") Then
        Dim altFile = Regex.Replace(origFile, "\.h$", ".cpp")
        If Not My.Computer.FileSystem.FileExists(altFile) Then
            altFile = Regex.Replace(origFile, "\.h$", ".cc")
        End If
        DTE.Documents.Open(altFile, "Text")
    End If
End Sub

' Finds which class the cursor is inside of, and returns the class name.'
' Note: This is indent based.  Your class bodies must be indented, and the closing'
' "};" must line up with the initial "class".'
' ex: class Foo {\nclass Bar {\n <cursor>...'
' returns: "Foo::Bar"''
Private Function ClassNameFinder() As String
    Dim sel As TextSelection = DTE.ActiveDocument.Selection
    Dim origPos = sel.ActivePoint.AbsoluteCharOffset
    Dim endLine = sel.CurrentLine

    sel.MoveToAbsoluteOffset(1, True)
    Dim pos = 0
    Dim text = sel.Text
    Dim className = ClassNameFinderInternal(text)
    sel.MoveToAbsoluteOffset(origPos)

    If className.Length > 0 Then
        className = className.Substring(2)
    End If
    Return className
End Function

' Helper function for ClassNameFinder.  Returns the full class name that doesn\'t'
' have matching close braces in the given text string.'
Private Function ClassNameFinderInternal(ByRef text As String) As String
    Dim className = ""
    While text.Length > 0
        Dim match = Regex.Match(text, "^( *)class ([^ \n\r{:;]+)[^\n\r;]*$", RegexOptions.Multiline)
        If match.Success Then
            Dim indentString = match.Groups.Item(1).Value
            Dim newClass = "::" + match.Groups.Item(2).Value
            text = text.Substring(match.Index + match.Length)

            match = Regex.Match(text, "^" + indentString + "};", RegexOptions.Multiline)
            If match.Success Then
                text = text.Substring(match.Index + match.Length)
            Else
                className += newClass + ClassNameFinderInternal(text)
            End If
        Else
            text = ""
            Exit While
        End If
    End While

    Return className
End Function
```


```
Sub OpenChangeListFiles()
    ' Open all the files of a given change list.'
    Dim change_list_name As String = InputBox("Enter the change list name." + vbNewLine + "(with an optional repo folder path prefix)")
    If String.IsNullOrEmpty(change_list_name) Then
        Exit Sub
    End If
    Dim solution As Solution = DTE.Solution
    ' Try to get the source root path starting from the solution path and search upward in the folder hierarchy.'
    Dim source_root_path As String
    If Not solution Is Nothing And Not String.IsNullOrEmpty(solution.FullName) Then
        source_root_path = GetRepositoryRootFolder(solution.FullName)
    End If
    If source_root_path Is Nothing Or String.IsNullOrEmpty(source_root_path) Then
        ' We couldn\'t find the root ourselves, ask the user.'
        source_root_path = InputBox("Can't find a solution file path." + vbNewLine + "Please specify the root of your source tree.")
        If String.IsNullOrEmpty(source_root_path) Then
            Exit Sub
        End If
    End If
    ' If we provided one or more \ in change_list_name, we want to check a subdirectory of the root path.'
    ' This is useful if we have another repository in our solution.'
    Dim change_list_path() As String = Split(change_list_name, "\")
    If change_list_path.Length > 1 Then
        source_root_path += "\" + String.Join("\", change_list_path, 0, change_list_path.Length - 1)
        change_list_name = change_list_path(change_list_path.Length - 1)
    End If
    ' Look for the CL file in the appropriate folder.'
    Dim change_list_file As String = source_root_path + "\.svn\gcl_info\changes\" + change_list_name
    If Not IO.File.Exists(change_list_file) Then
        ' OK, give one last chance to the user to specify the appropriate path for the CL.'
        source_root_path = InputBox("Can't find CL: '" + change_list_name + "', under " + source_root_path + vbNewLine + "Specify the proper root folder one last time:")
        If String.IsNullOrEmpty(source_root_path) Then
            Exit Sub
        End If
        change_list_file = source_root_path + "\.svn\gcl_info\changes\" + change_list_name
        If Not IO.File.Exists(change_list_file) Then
            MsgBox("Can't find CL: '" + change_list_name + "', under " + source_root_path)
            Exit Sub
        End If
    End If
    ' Now load its content.'
    Dim change_list_content As String
    Try
        change_list_content = IO.File.ReadAllText(change_list_file, Text.Encoding.GetEncoding(1252))
    Catch e As Exception
        MsgBox("Exception: " + e.Message())
        Exit Sub
    End Try
    ' Match the lines where the paths of the opened files can be found.'
    Dim pattern As String = "M\s*(.*)$"
    Dim regex As New Text.RegularExpressions.Regex(pattern, Text.RegularExpressions.RegexOptions.Multiline)
    Dim matches As Text.RegularExpressions.MatchCollection = regex.Matches(change_list_content)
    Dim match
    For Each match In matches
        ' And now we can open each and everyone of them.'
        Dim file_path As String = match.Groups(1).ToString()
        Dim full_path As String = source_root_path + "\" + Left(file_path, file_path.Length() - 1)
        If IO.File.Exists(full_path) Then
            DTE.ItemOperations.OpenFile(full_path)
        End If
    Next
End Sub

Private Function GetRepositoryRootFolder(ByVal solution_path As String) As String
    Try
        ' We look for a change in the svn: root path in the .svn/entries file.'
        ' This means we have reached view root or changed repo.'
        Dim solution_folder As String = IO.Directory.GetParent(solution_path).ToString()
        Dim svn_root_path As String = GetSvnRootPath(solution_folder)
        If String.IsNullOrEmpty(svn_root_path) Then
            ' We don'\t seem to be within a repo if we can\'t get the SVN root path.'
            Return ""
        End If
        ' We need to keep the previous path, since we need to stop once we found a bad parent.'
        Dim previous_path As String = solution_folder
        While True
            Dim next_path As String = IO.Directory.GetParent(previous_path).ToString()
            Dim current_svn_root_path As String = GetSvnRootPath(next_path)
            ' As long as we have the same svn root path, we are in the same repo, continue.'
            If current_svn_root_path = svn_root_path Then
                previous_path = next_path
            Else
                Exit While
            End If
        End While
        Return previous_path
    Catch e As Exception
        MsgBox("Exception: " + e.Message())
    End Try
    Return Nothing
End Function

Private Function GetSvnRootPath(ByVal client_path As String) As String
    ' First make sure we are within a repo.'
    Dim svn_folder As String = client_path + "\.svn"
    If Not IO.Directory.Exists(svn_folder) Then
        Return ""
    End If
    ' Then there MUST be an entries file in there.'
    Dim entries_file As String = svn_folder + "\entries"
    If Not IO.File.Exists(entries_file) Then
        Return ""
    End If
    ' Read the content of the file and find the svn root, and return it.'
    Dim entries_content As String = IO.File.ReadAllText(entries_file, Text.Encoding.GetEncoding(1252))
    Dim pattern As String = "svn:(.*)$"
    Dim regex As New Text.RegularExpressions.Regex(pattern, Text.RegularExpressions.RegexOptions.Multiline)
    Dim matches As Text.RegularExpressions.MatchCollection = regex.Matches(entries_content)
    Return matches.Item(1).ToString()
End Function

```


```

Sub BuildStartupProject()
    Dim sb As SolutionBuild = DTE.Solution.SolutionBuild
    Dim projName As String = sb.StartupProjects(0)
    DTE.ExecuteCommand("View.Output")
    sb.BuildProject(sb.ActiveConfiguration.Name, projName, False)
End Sub
```


```
Sub WinbgStartupProject()
    ' Use the right one:
    Dim windbg As String = "C:\program files\Debugging Tools for Windows (x86)\windbg.exe"
    'Dim windbg As String = "C:\program files\Debugging Tools for Windows (x64)\windbg.exe"

    Dim project_name As String = CType(DTE.Solution.SolutionBuild.StartupProjects(0), String)
    Dim project As EnvDTE.Project = FindProjects(DTE.Solution.Projects, project_name)
    Dim config As Configuration = project.ConfigurationManager.ActiveConfiguration
    ' Hack to remove file:///
    Dim target_path As String = config.OutputGroups.Item(1).FileURLs(0).ToString().Remove(0, 8)
    Dim arguments As String = config.Properties.Item("CommandArguments").Value
    'MsgBox(windbg & " -o " & target_path & " " & arguments)
    System.Diagnostics.Process.Start(windbg, "-o " & target_path & " " & arguments)
End Sub

Function FindProjectItems(ByVal project_items As EnvDTE.ProjectItems, ByVal project_name As String) As EnvDTE.Project
    FindProjectItems = Nothing
    For Each project_item As EnvDTE.ProjectItem In project_items
        If Not project_item.SubProject Is Nothing Then
            FindProjectItems = FindProject(project_item.SubProject, project_name)
            If Not FindProjectItems Is Nothing Then Exit Function
        End If
    Next
End Function

Function FindProject(ByVal project As EnvDTE.Project, ByVal project_name As String) As EnvDTE.Project
    If project.UniqueName = project_name Then
        FindProject = project
        Exit Function
    End If
    If Not project.ProjectItems Is Nothing Then
        FindProject = FindProjectItems(project.ProjectItems, project_name)
        If Not FindProject Is Nothing Then Exit Function
    End If
End Function

Function FindProjects(ByVal projects As EnvDTE.Projects, ByVal project_name As String) As EnvDTE.Project
    ' You never thought it'd be so complex to find a project. The VS extensibility team
    ' stole me an hour of my life I will never get back.
    FindProjects = Nothing
    For Each project As EnvDTE.Project In projects
        FindProjects = FindProject(project, project_name)
        If Not FindProjects Is Nothing Then Exit Function
    Next
End Function

```


```
  Public Sub keypress(ByVal key As String, ByVal sel As TextSelection, ByVal completion As Boolean, ByRef cancel As Boolean) _
            Handles TextDocumentKeyPressEvents.BeforeKeyPress
        If (Not completion And key = vbCr) Then
            Dim textDocument As TextDocument = DTE.ActiveDocument.Object("TextDocument")
            Dim startPoint As EditPoint = textDocument.StartPoint.CreateEditPoint()
            startPoint.MoveToLineAndOffset(sel.ActivePoint.Line, 1)
            Dim text = startPoint.GetText(sel.ActivePoint.LineCharOffset - 1)
            Dim pos = findUnclosedParenIndent(text)

            If pos <> -1 Then
                Dim commentPos = text.IndexOf("//")
                If commentPos = -1 Or commentPos > pos Then
                    sel.Insert(vbLf)
                    sel.DeleteWhitespace()
                    sel.PadToColumn(pos + 2)
                    cancel = True
                End If
            End If
        End If
    End Sub

    Public Function findUnclosedParenIndent(ByRef text As String) As Integer
        findUnclosedParenIndent = -1

        Dim parens As Char() = "()".ToCharArray()
        Dim lastPos = text.Length
        Dim numClosed = 0

        While True
            Dim pos = text.LastIndexOfAny(parens, lastPos - 1)

            If pos = -1 Then
                Exit While
            End If

            If text(pos) = ")" Then
                numClosed += 1
            Else
                If numClosed = 0 Then
                    findUnclosedParenIndent = pos
                    Exit While
                End If
                numClosed -= 1
            End If

            lastPos = pos
        End While
    End Function
```

```
$> mkdir android-accessories
$> cd android-accessories
$> repo init -u https://android.googlesource.com/accessories/manifest
$> repo sync
```


```
$> ./setup
```


```
$> ./build
```

```
$> ./flash
```


```
ADK L;
void setup() {
 L.adkInit();
 L.btStart();
}
...
void btStart(){
    uint8_t i, dlci;
    int f;

    L.btEnable(adkBtConnectionRequest, adkBtLinkKeyRequest, adkBtLinkKeyCreated,
               adkBtPinRequest, NULL);

    dlci = L.btRfcommReserveDlci(RFCOMM_DLCI_NEED_EVEN);

    if(!dlci) dbgPrintf("BTADK: failed to allocate DLCI\n");
    else{

        //change descriptor to be valid...
        for(i = 0, f = -1; i < sizeof(sdpDescrADK); i++){

            if(sdpDescrADK[i] == MAGIX){
                if(f == -1) f = i;
                else break;
            }
        }

        if(i != sizeof(sdpDescrADK) || f == -1){

            dbgPrintf("BTADK: failed to find a single marker in descriptor\n");
            L.btRfcommReleaseDlci(dlci);
            return;
        }

        sdpDescrADK[f] = dlci >> 1;

        dbgPrintf("BTADK has DLCI %u\n", dlci);

        L.btRfcommRegisterPort(dlci, btAdkPortOpen, btAdkPortClose, btAdkPortRx);
        L.btSdpServiceDescriptorAdd(sdpDescrADK, sizeof(sdpDescrADK));
    }
}

```


```
void loop(void)
{
  ...
  L.adkEventProcess(); //let the adk framework do its thing
  ...
}
```


```
mAdapter = BluetoothAdapter.getDefaultAdapter();
BluetoothDevice device = mAdapter.getRemoteDevice(address);
mSocket = device.createInsecureRfcommSocketToServiceRecord(ADK_UUID);
mSocket.connect();

```


```
mInStream = mSocket.getInputStream();
mOutStream = mSocket.getOutputStream();

```


```
ADK L;
void setup() {
  L.adkInit();
  L.usbSetAccessoryStringVendor(...);
  L.usbSetAccessoryStringName(...);
  L.usbSetAccessoryStringLongname(...);
  L.usbSetAccessoryStringVersion(...);
  L.usbSetAccessoryStringUrl(...);
  L.usbSetAccessoryStringSerial(...);

  L.usbStart();
}

```

```

void loop(void)
{
  ...
  L.adkEventProcess(); //let the adk framework do its thing
  ...
}

```


```
void loop() {
  if (L.accessoryConnected()) {
    int recvLen = L.accessoryReceive(msg, sizeof(msg));
    if (recvLen > 0) {
      ... // process message
    }

    L.accessorySend(outmsg, outmsgLen);
  }
  L.adkEventProcess();
}

```


```
import com.android.future.usb.UsbAccessory;
import com.android.future.usb.UsbManager;

mUSBManager = UsbManager.getInstance(this);
UsbAccessory acc = mUSBManager.getAccessoryList()[0];

if (!mUSBManager.hasPermission(acc)) return;

```

```
import android.hardware.usb.UsbAccessory
import android.hardware.usb.UsbManager

mUSBManager = (UsbManager) getSystemService(Context.USB_SERVICE);
UsbAccessory acc = (UsbAccessory)
                   intent.getParcelableExtra(UsbManager.EXTRA_ACCESSORY);

```


```
<meta-data
    android:name="android.hardware.usb.action.USB_ACCESSORY_ATTACHED"
    android:resource="@xml/usb_accessory_filter" />

```

```
ParcelFileDescriptor mFD = mUSBManager.openAccessory(acc);
if (mFD != null) {
  FileDescripter fd = mFD.getFileDescriptor();
  mIS = new FileInputStream(fd);  // use this to receive messages
  mOS = new FileOutputStream(fd); // use this to send commands
}

```

```
ADK L;
void setup() {
  L.audioInit();
  L.usbh_init()
  L.usbStart();
}

```

```
void loop(void)
{
 void loop(void)
{
  ...
  L.adkEventProcess(); //let the adk framework do its thing
  ...
}
```







```
gcloud auth login
```





```
gcloud dns managed-zone create --dns_name="example.com." --description="A test zone" examplezonename
```


```
gcloud dns records --zone=examplezonename edit
```

```
{
  "additions": [
    {
      "kind": "dns#resourceRecordSet",
      "name": "example.com.",
      "rrdatas": [
        "ns-cloud1.googledomains.com. dns-admin.google.com. 1 21600 3600 1209600 300"
      ],
      "ttl": 21600,
      "type": "SOA"
    },
    ...
    {
      INSERT YOUR ADDITIONS HERE
    }
  ],
  "deletions": [
    {
      "kind": "dns#resourceRecordSet",
      "name": "example.com.",
      "rrdatas": [
        "ns-cloud1.googledomains.com. dns-admin.google.com. 0 21600 3600 1209600 300"
      ],
      "ttl": 21600,
      "type": "SOA"
    },
    ...
    {
      INSERT YOUR DELETIONS HERE
    }
  ]
}

```









_source and blog_

http://source.android.com/accessories/aoa2.html


https://code.google.com/p/python-weather-api/wiki/Examples


http://www.cisco.com/cisco/psn/web/workspace


http://stuffandplayx.blogspot.com/


https://drive.google.com/folderview?id=0B2dYfTekxoleUl83UnNYQ1owM3c&usp=sharing


https://drive.google.com/folderview?id=0B2dYfTekxoleRUlDYWpSd0gzVW8&usp=sharing


https://drive.google.com/folderview?id=0B2dYfTekxoleYk9qblVzZWZIbXc&usp=sharing


https://drive.google.com/folderview?id=0B2dYfTekxoleVmpPWC1RRUwxWnM&usp=sharing


https://drive.google.com/folderview?id=0B2dYfTekxoleYkZRdUlmM2VLYnc&usp=sharing


https://drive.google.com/folderview?id=0B2dYfTekxoleX2hGbzJtUFZia28&usp=sharing


https://drive.google.com/folderview?id=0B2dYfTekxoledU1fcEx4SlNJdEE&usp=sharing



https://drive.google.com/folderview?id=0B2dYfTekxoleMDFxdWdiYXl1RU0&usp=sharing


https://drive.google.com/folderview?id=0B2dYfTekxoleTVBRWXB3bnhkVTQ&usp=sharing


https://drive.google.com/folderview?id=0B2dYfTekxoledWFhZEhRWWI1aFU&usp=sharing


https://goo.gl/maps/lwOln


http://www.chromium.org/developers/how-tos/debugging

https://developer.android.com/tools/adk/adk2.html#adk-conn

https://code.google.com/p/chromium/wiki/WindowsVisualStudioMacros#Automatically_grab_chromium_child_processes

https://developers.google.com/cloud-dns/getting-started


http://developer.android.com/index.html


https://dns.norton.com/index.html

## dns safe for ipv4 ##
### _199.85.126.10_ ###
### _199.85.126.20_ ###
### _199.85.126.30_ ###

## dns safe alternate for ipv4 ##
### _199.85.127.10_ ###
### _199.85.127.20_ ###
### _199.85.127.30_ ###