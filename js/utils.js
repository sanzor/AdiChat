export{showElement,hideElement};
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

