" Easy Complete You(ECY) completion plugin
"
" Copyright © 2019 JimmyHuang
" 2019-02-15 
" Under MIT license
" part of the codes was copy from YCM
" please obey the license of them.

if exists( "g:loaded_easycomplete" )
  finish
elseif v:version < 704 || (v:version == 704 && !has( 'patch1578' ))
  echohl WarningMsg |
        \ echomsg "EasyCompletion unavailable: requires Vim 7.4.1578+." |
        \ echohl None
  if v:version == 704 && has( 'patch8056' )
    " Very very special case for users of the default Vim on macOS. For some
    " reason, that version of Vim contains a completely arbitrary (presumably
    " custom) patch '8056', which fools users (but not our has( 'patch1578' )
    " check) into thinking they have a sufficiently new Vim. In fact they do
    " not and ECY fails to initialise. So we give them a more specific warning.
    echohl WarningMsg
          \ | echomsg
          \ "Info: You appear to be running the default system Vim on macOS. "
          \ . "It reports as patch 8056, but it is really older than 1578. "
          \ . "Please consider MacVim, homebrew Vim or a self-built Vim that "
          \ . "satisfies the minimum requirement."
          \ | echohl None
  endif
  finish
elseif !has( 'timers' )
  echohl WarningMsg |
        \ echomsg "EasyCompletion unavailable: requires Vim compiled with " .
        \ "the timers feature." |
        \ echohl None
  finish
elseif &encoding !~? 'utf-\?8'
  echohl WarningMsg |
        \ echomsg "EasyCompletion unavailable: supports UTF-8 encoding only. " .
        \ "Put the line 'set encoding=utf-8' in your vimrc." |
        \ echohl None
  finish
elseif !exists( "g:loaded_LSP_lib" )
  echohl WarningMsg |
        \ echomsg "EasyCompletion unavailable: no found LSP lib" .
        \ "Easycompletion depend on LSP lib of viml." |
        \ echohl None
  finish
endif

let g:running_servers_of_buffer={}
let g:running_fuzzy_find_job_id=0
let g:fuzzy_find_path="D:/gvim/vimfiles/myplug/EasyCompleteYou/FuzzyMatch/main.exe"
" e.g : {"1":{"start_position":{"line":34,"character":23}}}
let g:available_completion={}
let g:ECY_using_save_automatically=1
let s:waitting_completion_position={}
let s:WorkspaceClientCapabilities={  "applyEdit":v:true,  "workspaceEdit":{    "documentChanges":v:true,    "resourceOperations":[      "Create",      "Rename",      "Delete"    ],    "failureHandling":[      "Abort"    ]  },  "didChangeConfiguration":{    "dynamicRegistration":v:false  },  "didChangeWatchedFiles":v:false,  "symbol":{    "dynamicRegistration":v:false,    "symbolKind":{      "valueSet":[]    }  },  "executeCommand":{    "dynamicRegistration":v:false  },  "workspaceFolders":v:true,  "configuration":v:true}
let s:TextDocumentClientCapabilities={  "synchronization":{    "dynamicRegistration":v:false,    "willSave":v:false,    "willSaveWaitUntil":v:false,    "didSave":v:false  },  "completion":{    "dynamicRegistration":v:false,    "completionItem":{      "snippetSupport":v:true,      "commitCharactersSupport":v:true,      "documentationFormat":[],      "deprecatedSupport":v:true,      "preselectSupport":v:true    },    "completionItemKind":{      "valueSet":[]    },    "contextSupport":v:true  },  "hover":{    "dynamicRegistration":v:false,    "contentFormat":[]  },  "signatrueHelp":{    "dynamicRegistration":v:false,    "signatrueInformation":{      "documentationFormat":[],      "parameterInformation":{        "labelOffsetSupport":v:false      }    }  },  "references":{    "dynamicRegistration":v:true  },  "documentHighlight":{    "dynamicRegistration":v:true  },  "documentSymbol":{    "dynamicRegistration":v:true,    "symbolKind":{      "valueSet":[]    },    "hierarchicalDocumentSymbolSupport":v:true  },  "formatting":{    "dynamicRegistration":v:true  },  "rangeFormatting":{    "dynamicRegistration":v:true  },  "onTypeFormatting":{    "dynamicRegistration":v:true  },  "declaration":{    "dynamicRegistration":v:true,    "linkSupport":v:true,  },  "definition":{    "dynamicRegistration":v:true,    "linkSupport":v:true,  },  "typeDefinition":{    "dynamicRegistration":v:true,    "linkSupport":v:true,  },  "implementation":{    "dynamicRegistration":v:true,    "linkSupport":v:true,  },  "codeAction":{    "dynamicRegistration":v:true,    "codeActionLiteralSupport":{      "codeActionKind":{        "valueSet":[]      }    }  },  "codeLens":{    "dynamicRegistration":v:true  },  "documentLink":{    "dynamicRegistration":v:true  },  "colorProvider":{    "dynamicRegistration":v:true  },  "rename":{    "dynamicRegistration":v:true,    "prepareSupport":v:true,  },  "publishDiagnostics":{    "relatedInformation":v:true  },  "foldingRange":{    "dynamicRegistration":v:true,    "rangeLimit":v:true,    "lineFoldingOnly":v:true  }}

augroup youcompleteme
  autocmd!
  " autocmd FileType * call s:OnFileTypeSet()
  autocmd BufEnter *     call s:OnBufferEnter()
  autocmd BufLeave *     call s:OnBufferLeave()

  autocmd InsertEnter *  call s:OnInsertMode()
  " autocmd TextChanged *  call s:OnTextChangedNormalMode()

  " invoked after typed a character into the buffer
  autocmd TextChangedI * call s:OnTextChangedInsertMode()
augroup END


function! GetACharacterFromCurrenBufferOnInsertMode() abort
  let l:line_string=getline(".")
  if len(l:line_string)>1000
    return
  endif
  let l:line_list = split(l:line_string, '\zs') 
  if col('.')==1
    return ''
  else
    return get(l:line_list, col('.')-2, '') 
  endif
endfunction

function! GetCharacterFromCurrenBufer(line,colum) abort
  let l:line_string=getline(a:line)
  if len(l:line_string)>1000 || l:line_string==''
    return
  endif
  let l:line_list = split(l:line_string, '\zs') 
  if a:colum==1
    return ''
  else
    return get(l:line_list, col('.')-2, '') 
  endif

endfunction

function! s:OnTextChangedInsertMode() abort
"{{{
  let l:bufNr=bufnr('%')
  if !has_key(g:running_servers_of_buffer,l:bufNr)
    return
  endif
  let l:line_nr=line('.')
  for [job_id,value] in items(g:available_completion)

    " the complete can be used if there are CacheID
    if has_key(value,'position') 
      if value['position']['line']== l:line_nr- 1 
        let l:insertText=strpart(getline(l:line_nr),
              \value['position']['character'])

        if l:insertText=~'\W' || col('.') - 1<value['position']['character']
          call s:RefleshCompletion(l:bufNr)
        else
          if (has_key(value,'sortText')?(value['sortText']!=l:insertText):1)
            let g:available_completion[job_id]['sortText']=l:insertText
            call LSP_log(l:insertText)
            if has_key(value,'CacheID')
              call s:InvokeFuzzyMatch(value['request_id'],
                    \value['CacheID'],l:insertText)
            endif
          endif
        endif

        " calling flitter
      else
        call s:RefleshCompletion(l:bufNr)
      endif
    endif
  endfor
"}}}
endfunction

function! s:InvokeFuzzyMatch(request_id,CacheID,sortText) abort
  let l:msg={ "request_id":a:request_id,"CacheID":a:CacheID,
        \"method":"fliter","sortText":a:sortText }
  call LSP_log(l:msg)
  call async#job#send(g:running_fuzzy_find_job_id,json_encode(l:msg))
endfunction

function! s:OnTextChangedNormalMode() abort
  call s:RefleshText()
endfunction

function! s:RefleshText() abort
  if g:ECY_using_save_automatically==1
    silent! execute 'w!'
  else
    let l:bufNr=bufnr('%')
    if s:BufferHasChanged(l:bufNr) && 
          \has_key(g:running_servers_of_buffer,l:bufNr)
      for server_job_id in g:running_servers_of_buffer[l:bufNr]
        if !has_key(g:LSP_running_servers_job_id[server_job_id],
              \'ServerCapabilities')
          return
        endif
        call LSP_DidChangeNotification(server_job_id,{'uri':
              \LSP_path_to_uri(s:CurrenBufferPath()),'text':
              \Get_text_document_text(bufnr('%'))})
      endfor
    endif
  endif
endfunction

function! s:BufferHasChanged(bufNr) abort
  " but not saved
  let l:bufInfo=getbufinfo(a:bufNr)
  return l:bufInfo[0]['changed']
endfunction

function! s:OnBufferEnter() abort 
"{{{
  " matching every Suffix name in LSP_servers_info
  for [key1,value1] in items(g:LSP_servers_info)
    if has_key(value1,'whiteList')
      for value2 in value1['whiteList']
        if value2==&filetype

          let l:ClientCapability={'workspace':s:WorkspaceClientCapabilities,
                \'textDocument':s:TextDocumentClientCapabilities}
          let l:job_id= LSP_StartServer({'name':key1,
                \'ClientCapabilities':l:ClientCapability},
                \{'user_callback':function('g:StartServer_cb')})
          let l:bufNr=bufnr('%')
          if string(l:job_id)=~'E'
            echo l:job_id
          else
            if !has_key(g:running_servers_of_buffer,l:bufNr)
              let g:running_servers_of_buffer[l:bufNr]=[]
            endif
            let g:running_servers_of_buffer[l:bufNr]=
                  \add(g:running_servers_of_buffer[l:bufNr],l:job_id)
          endif
          break
        endif
      endfor
    endif
  endfor

  " opening fuzzy match server, it will be kept in back ground util vim is
  " closed.
  if g:running_fuzzy_find_job_id==0 
    if g:fuzzy_find_path!="" && executable(g:fuzzy_find_path)
      " for convenience, the stderr of fuzzy match will print to the stdout.
      let l:start_opts={
            \ 'on_stdout': function('s:FuzzyFind_cb'),
            \ 'on_stderr': function('s:FuzzyFind_cb'),
            \ 'on_exit':   function('s:FuzzyFind_cb'),
            \ }
      let g:running_fuzzy_find_job_id= 
            \async#job#start(g:fuzzy_find_path,l:start_opts)
    else
      echohl WarningMsg |
            \ echomsg "fuzzy match server unvailable:requires fuzzy match server ".
            \ "to provide fuzzy match." |
            \ echohl None
      finish
    endif
  endif
"}}}
endfunction

function! s:KillAllLSPServer(bufNr) abort
"{{{
  " cleaning all the LSP server.
  if !has_key(g:running_servers_of_buffer,a:bufNr)
    return
  endif
  for job_id in g:running_servers_of_buffer[a:bufNr]
    call KillServer(job_id,'')
  endfor
  unlet g:running_servers_of_buffer[a:bufNr]
"}}}
endfunction

function! s:OnBufferLeave() abort
"{{{
  call s:KillAllLSPServer(bufnr('%'))
"}}}
endfunction

function! s:OnInsertMode() abort
"{{{
  let l:bufNr=bufnr('%')
  if !has_key(g:running_servers_of_buffer,l:bufNr)
    return
  endif
  call s:RefleshCompletion(l:bufNr)
"}}}
endfunction

function! s:CleanAvailable_completion() abort
"{{{
  for [job_id,value] in items(g:available_completion)
    if has_key(value,"CacheID") && g:running_fuzzy_find_job_id!=0
      " killCompleteSource have no response
      call async#job#send(g:running_fuzzy_find_job_id,
            \'{"method":'.'"killCompleteSource",'.'"CacheID":'.
            \value["CacheID"]."}")
    endif
    unlet g:available_completion[job_id]
  endfor
"}}}
endfunction

function! s:RefleshCompletion_cb(bufNr,position,timer_id) abort
"{{{
  call s:CleanAvailable_completion()
  for job_id in g:running_servers_of_buffer[a:bufNr]
    let g:available_completion[job_id]={}

    " this means LSP server has been initialled
    if !has_key(g:LSP_running_servers_job_id[job_id],'ServerCapabilities')
      return
    endif

    call s:RefleshText()
    let g:available_completion[job_id]["position"]=a:position
    call LSP_CompleteRequest(job_id,
          \{'CompletionTriggerKind': 1,'triggerCharacter':'',
          \'uri':                    LSP_path_to_uri(s:CurrenBufferPath()),
          \'position':               a:position},
          \{'user_callback':         function('g:ReadyToComplete_cb')})
  endfor
"}}}
endfunction

function! s:RefleshCompletion(bufNr) abort
"{{{
  let l:line = getline('.')
  let l:start = col('.') - 1
  while l:start > 0 && l:line[l:start - 1] =~ '\a'
    let l:start -= 1
  endwhile
  let l:position={ 'line': line('.')-1, 'character': l:start }
  call LSP_log("character".l:start)
  if l:position!=s:waitting_completion_position
    let s:waitting_completion_position=l:position
    call timer_start(200, function('s:RefleshCompletion_cb', 
          \[a:bufNr,l:position]))
  endif
"}}}
endfunction


function! g:ReadyToComplete_cb(server_job_id,results) abort
"{{{
  " sending results to fuzzy find server to let server prepare.
  " the results return 'time out' if time out.
  if g:running_fuzzy_find_job_id!=0
    
    let l:results_string=json_encode(a:results)
    let l:msg="|".string(len(l:results_string)).l:results_string
    call LSP_log(l:msg)
    call async#job#send(g:running_fuzzy_find_job_id,l:msg)
    " if g:available_completion contain request_id is means aready send the 
    " complete items to the fuzzy server
    let g:available_completion[a:server_job_id]['request_id']=a:results['id']
  else
    echohl WarningMsg |
          \ echomsg "EasyCompletion unavailable: Fuzzy find server is dead" .
          \ "Easycompletion depend on the fuzzy find server that write in golang." |
          \ echohl None
    finish
  endif
"}}}
endfunction

function! g:StartServer_cb(server_job_id,results) abort
  "{{{
  " the results return 'time out' if time out.
  if g:ECY_using_save_automatically==1
    silent! execute 'w!'
  else
    call LSP_DidOpenNotification(a:server_job_id,{'uri':
          \LSP_path_to_uri(s:CurrenBufferPath()),'languageId':
          \'python','text':Get_text_document_text(bufnr('%'))})
  endif
  echo 'Started server successfully.'
  "}}}
endfunction

function! s:FuzzyFind_cb(job_id,results,envent) 
  "{{{
  let l:response_msg =join(a:results,"\n")
  if l:response_msg=="\n"
    return
  endif

  try
    call LSP_log("echo of FuzzyFind_cb:".string(l:response_msg))
    let l:response_json=json_decode(l:response_msg)
  catch
    call LSP_log("failed to decode")
    echohl WarningMsg |
          \ echomsg "EasyCompletion unavailable: There are something worse " .
          \ "between fuzzy find server with ECY. Please report this problem with print out. Thanks"|
          \ echohl None
    return
  endtry

  if has_key(l:response_json,'Hint')
    echohl l:response_json['Hint']
    return
  endif

  " the complete source is ready to go if there are CacheID
  if has_key(l:response_json,'CacheID') 

    let l:is_abandoned=1
    for [job_id,value] in items(g:available_completion)
      if has_key(value,'request_id')
        if value['request_id']==l:response_json['RequestID']
          " if there Items, it's means a flitter msg
          if has_key(l:response_json,'Items')
            let g:available_completion[job_id]['items']=l:response_json['Items']
            let g:available_completion[job_id]['Is_empty']=
                  \l:response_json['Is_empty']
            call s:ShowPopup()
          else
            let g:available_completion[job_id]['CacheID']=l:response_json['CacheID']
          endif
          let l:is_abandoned=0
        endif
        break
      endif
    endfor
    if l:is_abandoned==1
      " this request is abandoned
      call async#job#send(g:running_fuzzy_find_job_id,
            \'{"method":'.'"killCompleteSource",'.'"CacheID":'.
            \l:response_json['CacheID']."}")
    endif
  endif
  "}}}
endfunction

function! s:ShowPopup() abort
  set completefunc=EasyCompleteCompleteFunc
  call s:SendKeys( "\<C-X>\<C-U>\<C-P>" )
endfunction

function! EasyCompleteCompleteFunc( findstart, base ) 
"{{{
  if a:findstart
    for [job_id,value] in items(g:available_completion)
      if has_key(value,'items')
        call LSP_log("position:".string(value['position']['character']))
        return value['position']['character']
      endif
    endfor
    return 3
  endif
  let l:items_list={'words': [], 'refresh': 'always'}
  for [job_id,value] in items(g:available_completion)
    if has_key(value,'items') && value['Is_empty']!=1
      for value2 in value['items']
        let l:temp={}
        let l:temp['word']=value2['InsertText']
          let l:temp['abbr']=value2['Label']
        let l:temp['info']=value2['Documentation']
        let l:temp['menu']=s:GetItemKind(value2['Kind'])
        call add(l:items_list['words'], l:temp)
      endfor
    endif
  endfor
  if l:items_list['words']==[]
    return []
  else
    return l:items_list
  endif
"}}}
endfunction
function! s:GetItemKind(KindNr) abort
"{{{
  if a:KindNr==1
    return 'Text'
  endif
  if a:KindNr==2
    return 'Method'
  endif
  if a:KindNr==3
    return 'Function'
  endif
  if a:KindNr==4
    return 'Constructor'
  endif
  if a:KindNr==5
    return 'Field'
  endif
  if a:KindNr==6
    return 'Variable'
  endif
  if a:KindNr==7
    return 'Class'
  endif
  if a:KindNr==8
    return 'Interface'
  endif
  if a:KindNr==9
    return 'Module'
  endif
  if a:KindNr==10
    return 'Property'
  endif
  if a:KindNr==11
    return 'Unit'
  endif
  if a:KindNr==12
    return 'Value'
  endif
  if a:KindNr==13
    return 'Enum'
  endif
  if a:KindNr==14
    return 'Keyword'
  endif
  if a:KindNr==15
    return 'Snippet'
  endif
  if a:KindNr==16
    return 'Color'
  endif
  if a:KindNr==17
    return 'File'
  endif
  if a:KindNr==18
    return 'Reference'
  endif
  if a:KindNr==19
    return 'Folder'
  endif
  if a:KindNr==20
    return 'EnumMember'
  endif
  if a:KindNr==21
    return 'Constant'
  endif
  if a:KindNr==22
    return 'Struct'
  endif
  if a:KindNr==23
    return 'Event'
  endif
  if a:KindNr==24
    return 'Operator'
  endif
  if a:KindNr==25
    return 'TypeParameter'
  endif
"}}}
endfunction

function! s:SendKeys( keys )
  call feedkeys( a:keys, 'in' )
endfunction

function! s:CurrenBufferPath(...) abort
  "{{{
  let file = a:0 ? a:1 : @%
  if file =~# '^\a\a\+:' || a:0 > 1
    return call('CurrenBufferPath', [file] + a:000[1:-1])
  elseif file =~# '^/\|^\a:\|^$'
    return file
  else
    let full_path=fnamemodify(file, ':p' . 
          \(file =~# '[\/]$' ? '' : ':s?[\/]$??'))
    return full_path
  endif
  "}}}
endfunction

