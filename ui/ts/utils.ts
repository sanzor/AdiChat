export{showElement,hideElement,postDataAsync,getDataAsync,getItemFromStorage,setItemInStorage};
function showElement(elementId:string){
    var element=document.getElementById(elementId);
    if(element){
        element.style.display="block";
    }
}
function hideElement(elementId:string){    
    var element=document.getElementById(elementId);
    if(element){
        element.style.display="none";
    }
}

async function postDataAsync(url:string = "", data:object = {}) {
    try {
      var myHeaders = new Headers();
      myHeaders.append("Content-Type", "application/json");
      var d=JSON.stringify(data);
      var requestOptions:RequestInit = {
        credentials:"same-origin" as RequestCredentials,
        method: 'POST',
        headers: myHeaders,
        body: d,
        redirect: 'follow'
      };
      const response=await fetch(url,requestOptions);
        if(response.status==409){
            return {result:"error",reason:"user_already_exists"};
        }
          return response.json(); 
    }catch(error){
        console.error(error);
    }
  }

  async function getDataAsync(url:string=""){
    console.log(url);
    const response = await fetch(url, {
        method: "GET", // *GET, POST, PUT, DELETE, etc.
      //   mode: "no-cors", // no-cors, *cors, same-origin
        cache: "no-cache", // *default, no-cache, reload, force-cache, only-if-cached
        credentials: "same-origin", // include, *same-origin, omit
        redirect: "follow", // manual, *follow, error
        referrerPolicy: "no-referrer", // no-referrer, *no-referrer-when-downgrade, origin, origin-when-cross-origin, same-origin, strict-origin, strict-origin-when-cross-origin, unsafe-url
      });
      console.log(response);
      return response.json(); 
}

function getItemFromStorage<Result>(Key:string):Result|null{
  var item=localStorage.getItem(Key);
  console.log(item);
  if(item==null || item=="undefined"){
    return null;
  }
  return JSON.parse(item);
}
function setItemInStorage<T>(Key:string,Value:T){ localStorage.setItem(Key,JSON.stringify(Value));}

