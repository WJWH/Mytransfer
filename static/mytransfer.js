function refreshBackground(){
    //var body = document.getElementsByTagName('body')[0];
    var now = new Date().getTime(); //cache breaker
    var src = "url(background?rnd=" + now + ")"
    document.body.style.backgroundImage = src
}
 
var int=self.setInterval(refreshBackground, 10000);