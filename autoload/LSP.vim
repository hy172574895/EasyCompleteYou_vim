" LSP lib of vimL 
" MIT license
" Copyright © 2019 JimmyHuang
" 2019-01-21 17:11
" part of the code was copy from (https://github.com/prabirshrestha/vim-lsp), 
" please obey the license of them.

" if exists( "g:loaded_LSP_lib" )
"   finish
" endif
" echo g:LSP_RegisterServer({'name':'go-langserver','whiteList':['go']})
" let g:lsp_log_file='C:/Users/qwe/Desktop/1.txt'
function! LSP_log(message) abort
  if exists('g:lsp_log_file')
    call writefile([localtime() . ':' . json_encode(a:message)], g:lsp_log_file, 'a')
    " call writefile([strftime('%c') . ':' . a:message.'\n'], g:lsp_log_file, 'a')
  endif
endfunction

function! s:DicInit(Dic1,Dic2) abort
  "{{{
  "it is a optional option when the value except for 0
  for [key,value] in items(a:Dic1)
    if !has_key(a:Dic2,key)
      if value=='0'
        return ''
      else
        let a:Dic2[key]=v:null
      endif
    endif
  endfor
  return a:Dic2
  "}}}
endfunction

function! s:VaribleInit(name,default_value) abort
  "{{{
  if !exists(a:name)
    exec 'let '.a:name.'='.a:default_value
  endif"}}}
endfunction

function! s:ScriptInit() abort
  call s:VaribleInit('g:LSP_running_servers_job_id','{}')"{{{
  call s:VaribleInit('g:LSP_servers_info','{}')
  call s:VaribleInit('g:LSP_timeOutMilliseconds','7000')
  call s:VaribleInit('g:LSP_responMsg','{}')"}}}
  let g:loaded_LSP_lib=1
endfunction

function! Response_of_symbol(server_job_id,results)
  call LSP_log("testing:".string(a:results))
endfunction

function! s:LSP_InitalizeResponseHandle(server_job_id,results) abort
  let g:LSP_running_servers_job_id[a:server_job_id]['ServerCapabilities']=
        \a:results['result']['capabilities']
  let l:Job_info           = g:LSP_running_servers_job_id[a:server_job_id]
  let l:Job_info_callback  = l:Job_info['request_queue']
        \[a:results['id']]['callback']['user_callback']
  if l:Job_info_callback  != ''
    call l:Job_info_callback(a:server_job_id,a:results)
  endif
  " call LSP_DidOpenNotification(a:server_job_id,{'uri':LSP_path_to_uri('C:\Users\qwe\Desktop\my\practice\Code\python\main.py'),'languageId':'python','text':s:get_text_document_text(bufnr('%'))})

  " call LSP_DocumentSymbolsRequest(a:server_job_id,{'uri':LSP_path_to_uri('C:\Users\qwe\Desktop\my\practice\Code\python\main.py')},{'On_stdout':function('s:response_of_symbol')})
  " call LSP_CompleteRequest(a:server_job_id,{'uri':LSP_path_to_uri('C:\Users\qwe\Desktop\my\practice\Code\python\main.py'),'CompletionTriggerKind':1,'position':LSP_get_position()},{'on_stdout':function('s:response_of_symbol')})

  " call LSP_typeDefinitionRequest(a:server_job_id,{})
endfunction
"{{{
function! s:urlencode_char(c) abort
  return printf('%%%02X', char2nr(a:c))
endfunction
function! s:get_prefix(path) abort
  return matchstr(a:path, '\(^\w\+::\|^\w\+://\)')
endfunction
function! s:encode_uri(path, start_pos_encode, default_prefix) abort
  let l:prefix      = s:get_prefix(a:path)
  let l:path        = a:path[len(l:prefix):]
  if len(l:prefix) == 0
    let l:prefix    = a:default_prefix
  endif

  let l:result = strpart(a:path, 0, a:start_pos_encode)

  for i in range(a:start_pos_encode, len(l:path) - 1)
    " Don't encode '/' here, `path` is expected to be a valid path.
    if l:path[i]   =~# '^[a-zA-Z0-9_.~/-]$'
      let l:result  .= l:path[i]
    else
      let l:result  .= s:urlencode_char(l:path[i])
    endif
  endfor

  return l:prefix . l:result
endfunction

if has('win32') || has('win64')
  function! LSP_path_to_uri(path) abort
    if empty(a:path)
      return a:path
    else
      " You must not encode the volume information on the path if
      " present
      let l:end_pos_volume    = matchstrpos(a:path, '\c[A-Z]:')[2]
      if l:end_pos_volume    == -1
        let l:end_pos_volume  = 0
      endif
      return s:encode_uri(substitute(a:path, '\', '/', 'g'), 
            \l:end_pos_volume, 'file:///')
    endif
  endfunction
else
  function! LSP_path_to_uri(path) abort
    if empty(a:path)
      return a:path
    else
      return s:encode_uri(a:path, 0, 'file://')
    endif
  endfunction
endif

function! s:decode_uri(uri) abort
  let l:ret = substitute(a:uri, '+', ' ', 'g')
  return substitute(l:ret, '%\(\x\x\)',
        \ '\=printf("%c", str2nr(submatch(1), 16))', 'g')
endfunction
if has('win32') || has('win64')
  function! LSP_uri_to_path(uri) abort
    return substitute(s:decode_uri(a:uri[len('file:///'):]), '/', '\\', 'g')
  endfunction
else
  function! LSP_uri_to_path(uri) abort
    return s:decode_uri(a:uri[len('file://'):])
  endfunction
endif
"}}}
function! s:get_content_length(headers) abort
  "{{{
  for l:header in split(a:headers, "\r\n")
    let l:kvp       = split(l:header, ':')
    if len(l:kvp)  == 2
      if l:kvp[0] =~? '^Content-Length'
        return str2nr(l:kvp[1], 10)
      endif
    endif
  endfor
  return -1
  "}}}
endfunction


function! s:requires_eol_at_eof(buf) abort
  let l:file_ends_with_eol = getbufvar(a:buf, '&eol')
  let l:vim_will_save_with_eol = !getbufvar(a:buf, '&binary') &&
        \ getbufvar(a:buf, '&fixeol')
  return l:file_ends_with_eol || l:vim_will_save_with_eol
endfunction
function! Get_text_document_text(buf) abort
  let l:buf_fileformat = getbufvar(a:buf, '&fileformat')
  let l:eol = {'unix': "\n", 'dos': "\r\n", 'mac': "\r"}[l:buf_fileformat]
  return join(getbufline(a:buf, 1, '$'), l:eol).
        \(s:requires_eol_at_eof(a:buf) ? l:eol : '')
endfunction
function! LSP_get_position(...) abort
  return { 'line': line('.') - 1, 'character': col('.') -1 }
endfunction
"{{{
" function s:Insnert_responMsg()
function! s:ResponseOperate(name,text)
  if !exists(a:name)

  endif
endfunction
function! s:LSP_OnStdout(server_job_id, data, event) abort
  if !has_key(g:LSP_responMsg,a:server_job_id)
    let g:LSP_responMsg[a:server_job_id]={}
  endif
  " a:data is a list, using join func to make it in string
  let l:response_msg           = join(a:data,"\n")
  for [key,value] in items(g:LSP_responMsg[a:server_job_id])
    let l:left_content_length  = g:LSP_responMsg
          \[a:server_job_id][key]['left']
    if l:left_content_length  != 0 && l:response_msg!=''
      let g:LSP_responMsg[a:server_job_id][key]['content'].=
            \l:response_msg[:l:left_content_length]
      let g:LSP_responMsg
            \[a:server_job_id][key]['left']=
            \g:LSP_responMsg[a:server_job_id][key]['Length']-
            \len(g:LSP_responMsg
            \[a:server_job_id][key]['content'])
      let l:response_msg=l:response_msg[l:left_content_length:]
    endif
  endfor

  while 1 
    let l:header_end_index  = stridx(l:response_msg, "\r\n\r\n")
    if !(l:response_msg    =~ 'Content-Length:') || l:header_end_index < 0
      let l:log_message     = 'E010:no headers found(or an unkonw erro)'
      call LSP_log(l:log_message)
      break
    else 
      let l:headers        = l:response_msg[:l:header_end_index - 1]
      let l:content_length = s:get_content_length(l:headers)
      let l:content_string = l:response_msg[l:header_end_index + 4:]
      let l:sequence       = len(g:LSP_responMsg[a:server_job_id])+1

      let g:LSP_responMsg[a:server_job_id][l:sequence]={}
      let g:LSP_responMsg[a:server_job_id][l:sequence]['Length']=
            \l:content_length
      let l:left_content_length   = l:content_length-len(l:content_string)

      if l:left_content_length>0 
        "There are incomplete responMsg 
        let l:response_msg        = ''
      elseif l:left_content_length<0
        " there are more than one response
        let l:content_string      = l:response_msg
              \[l:header_end_index + 4:
              \l:header_end_index + 3+l:content_length]
        let l:response_msg        = l:response_msg
              \[l:header_end_index+l:content_length+4:]
        let l:left_content_length = 0
      else
        " a complete and pure response
        let l:response_msg        = ''
      endif
      " try
      "   let l:content_json=json_decode(l:content_string)
      " catch
      "   call LSP_log('failed to parse:'.l:content_string)
      " endtry
      let g:LSP_responMsg[a:server_job_id][l:sequence]['content'] =
            \l:content_string
      let g:LSP_responMsg[a:server_job_id][l:sequence]['left']    =
            \l:left_content_length
      call LSP_log(g:LSP_responMsg)
      " break
    endif
  endwhile
  for [key,value] in items(g:LSP_responMsg[a:server_job_id])
    let l:left_content_length  = g:LSP_responMsg
          \[a:server_job_id][key]['left']
    if l:left_content_length  == 0
      let l:content_string     =
            \g:LSP_responMsg[a:server_job_id][key]['content']
      try
        let l:content_json     = json_decode(l:content_string)
      catch
        call LSP_log('failed to parse:'.l:content_string)
      endtry
      call LSP_log('handling:'.l:content_string)
      let l:Job_info=g:LSP_running_servers_job_id[a:server_job_id]
      if has_key(l:content_json,'id') && 
            \has_key(l:Job_info['request_queue'],l:content_json['id'])
        " a request respone from server
        call LSP_log('handling id:'.l:content_json['id'])
        let l:Job_info_callback=l:Job_info['request_queue']
              \[l:content_json['id']]['callback']
        if !(has_key(l:Job_info_callback,'called') )
          let g:LSP_running_servers_job_id[a:server_job_id]
                \['request_queue'][l:content_json['id']]['callback']
                \['called']  = 1
          let l:Callback     = l:Job_info_callback['on_stdout']
          if l:Callback     != v:null
            call l:Callback(a:server_job_id,l:content_json)
          else
            let l:Callback   = l:Job_info_callback['user_callback']
            if l:Callback   != v:null
              call l:Callback(a:server_job_id,l:content_json)
            endif
          endif
          unlet g:LSP_running_servers_job_id[a:server_job_id]
                \['request_queue'][l:content_json['id']]
          unlet g:LSP_responMsg[a:server_job_id][key]
        else
          " the response of this request might be handled

        endif
      else
        " a notification or a request sent from server

      endif
    endif
  endfor
endfunction 
function! s:LSP_OnStderr(server_job_id, data, event) abort
  call LSP_log(a:data)
endfunction

function! s:LSP_OnExit(server_job_id, data, event) abort
  call LSP_log(a:data)
endfunction

"}}}
function! s:SendRequest(server_job_id, opts,callback) abort
  "{{{
  if !has_key(g:LSP_running_servers_job_id,a:server_job_id)
    return 'E004:There is no suitable job of servers to send message.'
  endif

  " here is a example of runing job info's stucture:
  " let g:LSP_running_servers_job_id={'1':{
  "             \'server_':'pyls','name':'python','request_sequence':'0',
  "             \'request_queue':{'1':{'callback':{'on_stdout':function(xxx)},
  "             \'method':'initialize'}
  "             \}
  "             \}}
  let l:request_id            = g:LSP_running_servers_job_id[a:server_job_id]
        \['request_sequence'] + 1
  let g:LSP_running_servers_job_id[a:server_job_id]
        \['request_sequence'] = l:request_id
  let l:request_content={ 'jsonrpc': '2.0', 'method': a:opts['method'] ,
        \'id':l:request_id}
  let g:LSP_running_servers_job_id[a:server_job_id]['request_queue']
        \[l:request_id]={}
  let g:LSP_running_servers_job_id[a:server_job_id]['request_queue']
        \[l:request_id]['callback'] = s:DicInit(
        \{'on_stdout':'1','user_callback':'1'},a:callback)
  let g:LSP_running_servers_job_id[a:server_job_id]['request_queue']
        \[l:request_id]['method']   = a:opts['method']
  if has_key(a:opts, 'params')
    let l:request_content['params'] = a:opts['params']
  endif

  let l:request_content = json_encode(l:request_content)
  let l:request_header  = 'Content-Length: ' . len(l:request_content)
  let l:request         = l:request_header . "\r\n\r\n" . l:request_content
  call LSP_log('sending out------>:'.l:request)
  call async#job#send(a:server_job_id, l:request)
  " let l:timer_id= timer_start(
  "       \g:LSP_timeOutMilliseconds,function('s:TimeOutHandler'))

  " let g:LSP_running_servers_job_id[a:server_job_id]['request_queue']
  "       \[l:request_id]['timer_id']   = l:timer_id
  "}}}
endfunction

function! s:TimeOutHandler(timer_id) abort
  "{{{
  for [key1,value1] in items(g:LSP_running_servers_job_id)

    for [key2,value2] in items(value1['request_queue'])
      let l:Job_info_callback=value2['callback']

      if !has_key(l:Job_info_callback,'called')
        let l:Job_info_callback['called']          = 2
        if l:Job_info_callback['on_stdout']       != ''
          call LSP_log(string(key2).'timeout, job_id:'.string(key1))
          call l:Job_info_callback['on_stdout'](key1,'Time_out')
        else
          if l:Job_info_callback['user_callback'] != ''
            call l:Job_info_callback['user_callback']
                  \(key1,'Time_out')
            call LSP_log(string(key2).'timeout, job_id:'.string(key1))
          else
            call LSP_log(string(key2).'timeout,user have not 
                  \set callback, job_id:'.string(key1))
          endif
        endif
      else
      endif
      unlet g:LSP_running_servers_job_id[key1]
            \['request_queue'][key2]
      unlet g:LSP_responMsg[key1][key2]
    endfor
  endfor
  " }}}
endfunction
function! s:SendNotification(server_job_id, opts) abort
  "{{{
  let l:request_content={ 'jsonrpc': '2.0', 'method': a:opts['method']}
  let l:request_content['params'] = a:opts['params']
  let l:request_content = json_encode(l:request_content)
  let l:request_header  = 'Content-Length: ' . len(l:request_content)
  let l:request         = l:request_header . "\r\n\r\n" . l:request_content
  call async#job#send(a:server_job_id, l:request)
  call LSP_log(l:request)
  " }}}
endfunction

function! LSP_CancellationRequest(server_job_id,callback) abort
  "{{{
  let l:msg={'method':'$/cancelReques'}
  call s:SendRequest(a:server_job_id,l:msg,a:callback)
  "}}}
endfunction
function! LSP_typeDefinitionRequest(server_job_id,opts,callback) abort
  "{{{
  let l:opts=s:DicInit({'position':'0','uri':'0'}
        \,a:opts)
  if string(l:opts)==""
    return 'E001:Not enough parameters.'
  endif
  let l:textDocument={'uri':l:opts['uri']}
  " let l:textDocument={'uri':LSP_path_to_uri('C:\Users\qwe\Desktop\my\practice\Code\python\main.py')}
  let l:msg={'method':'textDocument/definition','params':{
        \'textDocument':l:textDocument,'position':l:opts['position']}}
  call s:SendRequest(a:server_job_id,l:msg,a:callback)
  "}}}
endfunction

function! LSP_ClientWorkspaceClientCapabilities(opts) abort
  "{{{
  let l:WorkspaceClientCapabilities={'applyEdit':l:opts['applyEdit',
        \'workspaceEdit':{'documentChanges':l:opts['documentChanges'],
        \'resourceOperations':l:opts['resourceOperations'],
        \'failureHandling':l:opts['failureHandling']}]}
  for [key,value] in items(a:Dic1)
    if !has_key(a:Dic2,key)
      if value=='0'
        return ''
      else
        let a:Dic2[key]=''
      endif
    endif
  endfor
  return a:Dic2
  "}}}
endfunction
function! LSP_InitializeRequest(server_job_id, opts,callback,capability) abort
  "{{{
  let l:opts=s:DicInit({'processId':'1','rootPath':'1',
        \'rootUri':'1','initializationOptions':'1',
        \'capabilities':'1','trace':'1','workspaceFolders':'1'
        \}
        \,a:opts)
  if string(l:opts)==""
    return 'E001:Not enough parameters.'
  endif
  let l:opts['capabilities']=a:capability
  let l:msg={'method':'initialize','params': l:opts }
  call s:SendRequest(a:server_job_id,l:msg,a:callback)
  "}}}
endfunction
function! LSP_DocumentSymbolsRequest(server_job_id, opts,callback) abort
  "{{{
  let l:opts           = s:DicInit({'uri':'0'},a:opts)
  if string( l:opts ) == ""
    return 'E001:Not enough parameters.'
  endif
  let l:params = {'textDocument':{'uri':l:opts['uri']}}
  let l:msg    = {'method':'textDocument/documentSymbol','params': l:params}
  call s:SendRequest(a:server_job_id,l:msg,a:callback)
  "}}}
endfunction
function! LSP_DidChangeConfigurationNotification(server_job_id, opts) abort
  "{{{
  let l:opts         = s:DicInit({'settings':'0'},a:opts)
  if string(l:opts) == ""
    return 'E001:Not enough parameters.'
  endif
  let l:params       = {'method':'initialize','params': l:opts }
  call s:SendNotification(a:server_job_id,l:params)
  "}}}
endfunction
function! LSP_DidChangeWorkspaceFoldersNotification(server_job_id, opts) abort
  "{{{
  let l:opts=s:DicInit({'addUri':'1','addName':'1',
        \'removedUri':'1','removeName':'1'
        \}
        \,a:opts)
  if string(l:opts)==""
    return 'E001:Not enough parameters.'
  endif
  let l:addWorkspaceFolder     = {'uri':l:opts['uri'],'name':l:opts['addName']}
  let l:removedWorkspaceFolder = {'uri':l:opts['uri'],
        \'name':l:opts['removeName']}
  let l:params={'method':'initialize','params': {'added':l:addWorkspaceFolder,
        \'removed':l:removedWorkspaceFolder} }
  call s:SendNotification(a:server_job_id,l:params)
  "}}}
endfunction
function! LSP_InitializedNotification(server_job_id, opts,callback) abort
  "{{{
  let l:opts=s:DicInit({'processId':'1','rootPath':'1',
        \'rootUri':'1','initializationOptions':'1',
        \'capabilities':'0','trace':'1'
        \}
        \,a:opts)
  if string(l:opts)==""
    return 'E001:Not enough parameters.'
  endif
  let l:msg={'method':'initialized','params': l:opts }
  call s:SendRequest(a:server_job_id,l:msg,a:callback)
  "}}}
endfunction
function! LSP_InitalizedNotification(server_job_id, opts) abort
  "{{{
  let l:msg={'method':'initialized','params':a:opts}
  call s:SendNotification(a:server_job_id,a:opts)
  "}}}
endfunction
function! LSP_DidChangeNotification(server_job_id, opts) abort
  "{{{
  let l:opts=s:DicInit({'uri':'0','range':'1',
        \'rangeLength':'1','text':'0','version':'1','position':'1'}
        \,a:opts)
  if string(l:opts)==""
    return 'E001:Not enough parameters.'
  endif
  " the range and rangelength should be omitted in vim, we have no idea how to 
  " calculate the different text in vim. If so it would be too slow to
  " calculate.
  "
  let l:content_changes={'range':l:opts['range'],
        \'rangeLength': l:opts['rangeLength'],
        \'text':        l:opts['text']}
  " let l:content_changes={'text':l:opts['text']}
  let l:VersionedTextDocumentIdentifier={'textDocument':
        \{'uri':     l:opts['uri']},
        \'version':  l:opts['version'],
        \'position': l:opts['position']}
  let l:params={'contentChanges':[l:content_changes],
        \'textDocument':l:VersionedTextDocumentIdentifier}
  let l:msg={'method':'textDocument/didChange','params':l:params}
  call s:SendNotification(a:server_job_id,l:msg)
  "}}}
endfunction
function! LSP_CompleteRequest(server_job_id,opts,callback) abort
  let l:opts=s:DicInit({'CompletionTriggerKind':'0','triggerCharacter':'1',
        \'uri':'0','position':'0'},
        \a:opts)
  if string(l:opts)==""
    return 'E001:Not enought parameters.'
  endif
  let l:params = {'context':{'triggerKind': l:opts['CompletionTriggerKind'],
        \'triggerCharacter': l:opts['triggerCharacter']},
        \'textDocument':{'uri': l:opts['uri']},
        \'position': l:opts['position']}
  let l:msg={'method':'textDocument/completion','params': l:params }
  call s:SendRequest(a:server_job_id,l:msg,a:callback)
endfunction

function! LSP_DidOpenNotification(server_job_id, opts) abort
  "{{{
  let l:opts=s:DicInit({'uri':'0','languageId':'0',
        \'version':'1','text':'0'}
        \,a:opts)
  if string(l:opts)==""
    return 'E001:Not enough parameters.'
  endif
  let l:params={'textDocument':{'uri':l:opts['uri'],'languageId':
        \l:opts['languageId'],'text':l:opts['text'],
        \'version':l:opts['version']}}
  let l:msg={'method':'textDocument/didOpen','params':l:params}
  call s:SendNotification(a:server_job_id,l:msg)
  "}}}
endfunction

function! LSP_DidCloseNotification(server_job_id, opts) abort
  "{{{
  let l:opts=s:DicInit({'uri':'0'},a:opts)
  if string(l:opts)==""
    return 'E001:Not enough parameters.'
  endif
  let l:params=('textDocument':{'uri':l:opts['uri']})
  let l:msg={'method':'textDocument/didClose','params':l:params}
  call s:SendNotification(a:server_job_id,l:msg)
  "}}}
endfunction
function! LSP_ExitNotification(server_job_id, opts) abort
  "{{{
  let l:msg={'method':'exit','params':a:opts}
  call s:SendNotification(a:server_job_id,a:opts)
  "}}}
endfunction
function! KillServer(server_job_id,callback)
  "{{{
  "return 1 if with erro, otherwise is 0.
  "there is no options
  call  async#job#stop(a:server_job_id)
  unlet g:LSP_responMsg[a:server_job_id]
  unlet g:LSP_running_servers_job_id[a:server_job_id]
  if a:callback!=''
    call a:callback()
  endif
  " let l:msg={'method':'exit' }
  " call s:SendRequest(a:server_job_id,l:msg,a:callback)

  "}}}
endfunction
function! LSP_ShutdownRuquest(server_job_id,callback)
  "{{{
  let l:msg={'method':'initialize','params': l:opts }
  call s:SendRequest(a:server_job_id,l:msg,a:callback)
  "}}}
endfunction
function! g:LSP_RegisterServer(server_info) abort
  "{{{
  " 'name',            requires;the name of implementation
  " 'server_location', optional;path to server's implementation
  " 'cmd',             optional;add to server when start
  " 
  " return all the register servers.
  let l:server_info=s:DicInit({'name':'0','server_location':'1',
        \'whiteList':'1','cmd':'1','InitializationOptions':1}
        \,a:server_info)
  if string(l:server_info)==''
    return 'E001:Not enough parameters.'
  endif

  let l:server_   = l:server_info['name']
  if !executable(l:server_)
    let l:server_ = l:server_info['server_location']
    if !executable(l:server_) 
      return 'E002:Failed to run ['.l:server_info['name'].']'
    endif
  endif

  let g:LSP_servers_info[l:server_info['name']]={
        \'server_':               l:server_,
        \'whiteList':             l:server_info['whiteList'],
        \'InitializationOptions': l:server_info['InitializationOptions'],
        \'cmd':                   l:server_info['cmd']}
  return g:LSP_servers_info
  "}}}
endfunction
function! LSP_StartServer(opts,callback) abort
  "{{{
  " start a server and send a initialize message to it.
  " the results of this function is up to init response message.
  let l:opts     = s:DicInit({'name':'0','rootUri':'1','workspaceFolders':'1',
        \'trace':'1','processId':'1'},a:opts)
  let l:callback = s:DicInit({'user_callback':'1',
        \'on_stderr':'1',
        \'on_exit':'1','on_stdout':'1'}
        \,a:callback)
  if string(l:opts)=="" || string(l:callback)==""
    return 'E001:Not enough parameters.'
  endif
  if !has_key(g:LSP_servers_info, l:opts['name'])
    return 'E003:There is not such a server, you should register it first.'
  endif
  let l:start_server=g:LSP_servers_info[l:opts['name']]

  let l:start_opts={
        \ 'on_stdout': function('s:LSP_OnStdout'),
        \ 'on_stderr': function('s:LSP_OnStderr'),
        \ 'on_exit':   function('s:LSP_OnExit'),
        \ }
  let l:server_id  = async#job#start(l:start_server.cmd,l:start_opts)

  if l:server_id  == -1
    call LSP_log("E006:Failed to start a job to run a server.")
    return 'E006:Failed to start a job to run a server.' .l:start_server.server_
  endif
  let l:start_opts={
        \ 'server_':          l:start_server['server_'] ,
        \ 'request_sequence': 0,
        \ 'request_queue':    {},
        \ 'name':             l:opts['name'] 
        \}
  let g:LSP_running_servers_job_id[ l:server_id ]=l:start_opts
  let l:start_opts={'initializationOptions':g:LSP_servers_info
        \[l:opts['name']]['InitializationOptions'],
        \'rootUri':l:opts['rootUri'],
        \'processId':l:opts['processId'],
        \'trace':l:opts['trace'],
        \'workspaceFolders':l:opts['workspaceFolders']}
  call LSP_InitializeRequest(l:server_id,l:start_opts,
        \{'on_stdout':function('s:LSP_InitalizeResponseHandle'),
        \'user_callback':l:callback['user_callback']},
        \l:opts['ClientCapabilities'])
  return l:server_id
  "}}}
endfunction
call s:ScriptInit()
