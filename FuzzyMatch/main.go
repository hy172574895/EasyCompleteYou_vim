package main

import (
  "fmt"
  "io"
  "os"
  "strconv"
  "strings"
  "github.com/bitly/go-simplejson"
  "encoding/json"
)
type itemsInfo struct{
  TextEdit      string
  Label         string
  Detail        string
  Documentation string
  InsertText    string
  Kind          int
  MatchGoal     int
}
type sortResults struct{
  SortText   string
  Version    int
  ID         int
  MatchItems []itemsInfo
}
type source struct{
  SourceItems []itemsInfo
  SortList    []sortResults
  Used        int
}
type FilterResults struct{
  Items     []itemsInfo
  RequestID int
  CacheID   int
  Is_empty  int
}
type initResults struct{
  RequestID int
  CacheID   int
}
type HintMsg struct{
  Hint     string
  ErroCode int
}

// only and might be hub
var RequestList    []source
var content        string
var Content_length int
var Hint           HintMsg


func main(){
  // {{{
  for {
    fmt.Println()
    data, err := ReadFrom(os.Stdin, 4100)
    data_string:=string(data)
    // data_string:=`|2095{"jsonrpc":"2.0","id":45,"result":{"iscomsdf":false,"items":[{"insertText":"sdfsdghsdlfg","label":"abc","kind":4,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"sdg","label":"gsdrfgoi","kind":8,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"},{"insertText":"wtiwghd;","label":"asdf","kind":9,"detail":"buzhidao"}]}}`
    if err!=nil {
      // handle erro when reading from stdin
    }else{
      if data_string  == "exit" {
        Hint.ErroCode  = 0
        Hint.Hint      = "exited"
        NormalPrintOut(Hint)
        break
      }
      Loop:
      if Content_length != 0 {
        left_length     := Content_length-len(content)
        switch{
        case len(data_string)<left_length:
          content       += data_string
          data_string    = "nil"
          Hint.ErroCode  = 0
          Hint.Hint     = "waitting for next buffer to arrive1."
          NormalPrintOut(Hint)

          // incomplete msg, watting for next buffer to arrive
        case len(data_string)==left_length:
          content        += data_string
          Content_length  = 0
          data_string     = "nil"
          HandleNewInitRequest(content)
        case len(data_string)>left_length:
          // Hint.ErroCode = 0
          // Hint.Hint     = "bigger"
          // NormalPrintOut(Hint)          
          HandleNewInitRequest(content)
          data_string   = data_string[left_length+1:]
          // handle by next
        }
      }
      if data_string[0]=='|' {
        Content_start     := strings.Index(data_string, "{")
        Content_length,_   = strconv.Atoi(data_string[1:Content_start])
        if Content_length == 0 {
          fmt.Printf("failed to get length.")
          break
        }else{
          switch{
          case Content_length>len(data_string[Content_start:]):
            Hint.ErroCode = 0
            Hint.Hint     = "waitting for next buffer to arrive."
            NormalPrintOut(Hint)           
            content       = data_string[Content_start:]
          case Content_length==len(data_string[Content_start:]):
            // Hint.ErroCode  = 0
            // Hint.Hint      = "it's a complete msg."
            // NormalPrintOut(Hint)           
            content        = data_string[Content_start:]
            HandleNewInitRequest(content)
            Content_length = 0
            data_string    = ""
          case Content_length<len(data_string[Content_start:]):
            // contain another msg

            // Hint.ErroCode  = 0
            // Hint.Hint      = "it's contain another msg."
            // NormalPrintOut(Hint)           
            temp           := Content_start+Content_length
            content         = data_string[Content_start:temp]
            data_string     = data_string[Content_start+Content_length:]
            HandleNewInitRequest(content)
            Content_length  = 0
            goto Loop
          } 
        }
      }else if data_string[0]=='{'{
        // {"request_id":0,"sortText":"cd","CacheID":0,"method":"fliter"}
        js, erro := simplejson.NewJson(data)
        if erro!=nil {
          fmt.Printf("failed to encode json.")
          break
        } else {
          CacheID,_:=js.Get("CacheID").Int()
          if CacheID>len(RequestList)-1 {
            Hint.ErroCode = 1
            Hint.Hint     = "CacheID is out of index."
            NormalPrintOut(Hint)          
            break
          }
          request_id,_:=js.Get("request_id").Int()
          switch js.Get("method").MustString() {
          case "fliter":
            Filter_2(js.Get("sortText").MustString(),CacheID,request_id)
          case "killCompleteSource":
            KillCompleteSource(CacheID)
          }
        }
      }
    }
  }

}
// }}}
func KillCompleteSource(CacheID int)error {
  var temp source
  temp.Used            = 0
  RequestList[CacheID] = temp
  return nil
  
}

func Filter_2(SortText string,CacheID int,feedback_id int) error {
  // {{{
  var oder_list      []itemsInfo
  var new_item       itemsInfo
  var Label          string
  var matching_start int
  var goal           int
  var max_list       int
  var result         FilterResults
  var b              string

  for i:=0;i<len(RequestList[CacheID].SourceItems);i++{
    Label     = RequestList[CacheID].SourceItems[i].Label
    matching_start = 0
    goal           = 0
    if len(SortText)<=len(Label) {
      for j  := 0;j<len(SortText);j++{
        for a:=0;a<2;a++{
          if a==0 {
            b=strings.ToUpper(string(SortText[j]))
          }else{
            b=string(SortText[j])
          }
          k    := strings.Index(Label[matching_start:],b)
          if k == -1 {
            if a==1{
              goal           += 100
            }
          }else{
            goal           += k
            matching_start += (k+1)
            break
          }
        }
      }
      new_item.InsertText    = RequestList[CacheID].SourceItems[i].InsertText
      new_item.Documentation = RequestList[CacheID].SourceItems[i].Documentation
      new_item.Kind          = RequestList[CacheID].SourceItems[i].Kind
      new_item.Label         = RequestList[CacheID].SourceItems[i].Label
      new_item.Detail        = RequestList[CacheID].SourceItems[i].Detail
      new_item.TextEdit      = RequestList[CacheID].SourceItems[i].TextEdit
      new_item.MatchGoal     = goal
      
      if len(oder_list)==0 {
        oder_list=append(oder_list,new_item) 
      }else{
        max_list=10
        
        var m int
        for m=len(oder_list)-1;m>=0;m--{
          if oder_list[m].MatchGoal<goal {
            // m+1 is this new item position
            if m==len(oder_list)-1 {
              if len(oder_list)<max_list {
                oder_list=append(oder_list,new_item)
              }
              break
            }
            var tempary []itemsInfo
						tempary   = append(tempary, oder_list[m+1:]...)
						oder_list = append(oder_list[:m+1], new_item)
						oder_list = append(oder_list, tempary...)
						break
          }
          if m==0 {
            var temp []itemsInfo
            temp      = append(temp, new_item)
            oder_list = append(temp, oder_list...)
          }
        }
        if len(oder_list)>max_list {
          oder_list=append(oder_list[:max_list-1])
        }
      }

    }
  }
  result.Items     = oder_list
  if len(result.Items)==0 {
    result.Is_empty=1
  }else{
    result.Is_empty=0
  }
  result.RequestID = feedback_id
  result.CacheID   = CacheID
  json,err := json.Marshal(result)
  if err   != nil {
    Hint.ErroCode = 1
    Hint.Hint     = "faild to encode json."
    NormalPrintOut(Hint)          
  }
  fmt.Print(string(json))
  return nil
}
// }}}

func Filter_1(SortText string,CacheID int,feedback_id int) error {
  // {{{
  // sortText_length:=len(SortText)
  // var suitableCacheID int
  // var suitableCacheLength int
  // // find the suitable Cache to improve the speed
  // for i:=len(RequestList[CacheID].SortList);i!=0;i--{
  //   sortText_chache:=RequestList[CacheID].SortList[i].SortText
  //   if len(sortText_chache)<=sortText_length {
  //     if SortText[:len(sortText_chache)]==sortText_chache  {
  //       if suitableCacheLength<len(sortText_chache) {
  //         suitableCacheID=i
  //         suitableCacheLength=len(sortText_chache)
  //       }
  //     }
  //   }
  // }

  // for i,_:=range RequestList[CacheID].SortList{

  // }
  return nil


}
// }}}

func HandleNewInitRequest(Request_content string) error{
  // {{{
  js, erro := simplejson.NewJson([]byte(Request_content))

  if erro!=nil {
    Hint.ErroCode = 1
    Hint.Hint     = "faild to encode json."
    NormalPrintOut(Hint)          
    return nil
  }
  var Asourse     source
  var Items       itemsInfo
  var SourceItems []itemsInfo
  var Aresulet    initResults
  var CacheID     int

  results, _ := js.Get("result").Get("items").Array()
  for i,_    := range results{
    item                := js.Get("result").Get("items").GetIndex(i)
    Items.Label          = item.Get("label").MustString()
    Items.Kind,_         = item.Get("kind").Int()
    Items.InsertText     = item.Get("insertText").MustString()
    Items.Documentation  = item.Get("documentation").MustString()
    Items.Detail         = item.Get("detail").MustString()
    Items.TextEdit       = item.Get("TextEdit").MustString()
    SourceItems          = append(SourceItems,Items)
  }
  Asourse.SourceItems = SourceItems
  // Asourse.SortList is empty, it's rely on user's filter
  Asourse.Used        = 1
  for j:=0;j<=len(RequestList);j++{
    if j==len(RequestList) {
      RequestList = append(RequestList,Asourse)
      CacheID     = len(RequestList)-1
      break
    }
    if RequestList[j].Used != 1 {
      RequestList[j]        = Asourse
      CacheID               = j
      break
    }

  }
  // the request id is up to LSP request id
  Aresulet.RequestID,_ = js.Get("id").Int()
  // this id will send back to user, user can send a request with this cacheID
  // to delete a source
  Aresulet.CacheID     = CacheID
  json,err:=json.Marshal(Aresulet)
  if err != nil {
    Hint.ErroCode = 1
    Hint.Hint     = "faild to encode json."
    NormalPrintOut(Hint)          
    return nil
  }
  fmt.Print(string(json))
  return nil
}
// }}}
func ReadFrom(reader io.Reader, num int) ([]byte, error) {
  p      := make([]byte, num)
  n, err := reader.Read(p)
  if n > 0 {
    return p[:n], nil
  }
  return p, err
}
func NormalPrintOut(msg HintMsg){
  returnMsg,_:=json.Marshal(msg)
  // returnMsg_len:=strconv.Itoa(len(returnMsg))
  fmt.Print(string(returnMsg))
  // fmt.Print("|"+returnMsg_len+string(returnMsg))
}
