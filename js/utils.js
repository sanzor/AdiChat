export{showElement,hideElement,postData,getDataAsync};
function showElement(elementId){
    var element=document.getElementById(elementId);
    if(element){
        element.style.display="block";
    }
}
function hideElement(elementId){    
    var element=document.getElementById(elementId);
    if(element){
        element.style.display="none";
    }
}

async function postData(url = "", data = {}) {
    try {
        const response = await fetch(url, {
            method: "POST", // *GET, POST, PUT, DELETE, etc.
          //   mode: "no-cors", // no-cors, *cors, same-origin
            cache: "no-cache", // *default, no-cache, reload, force-cache, only-if-cached
            credentials: "same-origin", // include, *same-origin, omit
            headers: {
              "Content-Type": "application/json",
              // 'Content-Type': 'application/x-www-form-urlencoded',
            },
            redirect: "follow", // manual, *follow, error
            referrerPolicy: "no-referrer", // no-referrer, *no-referrer-when-downgrade, origin, origin-when-cross-origin, same-origin, strict-origin, strict-origin-when-cross-origin, unsafe-url
            body: JSON.stringify(data), // body data type must match "Content-Type" header
          });
          console.log(response);
          if(response.status==409){
            return {result:"error",reason:"user_already_exists"};
          }
          return response.json(); 
    }catch(error){
        console.error(error);
    }
  }

  async function getDataAsync(url=""){
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

