$(document).ready(function(){
	
	disableGroupButtons(1, 2, "Adj", true); //Disable all
	disableGroupButtons(1, 2, "AdjA", true); //Disable all
	disableGroupButtons(1, 2, "AdjB", true); //Disable all
	disableSingleButtons("Advanced", true); //Disable all
	disableSingleButtons("Advanced", true); //Disable all
	disableSingleButtons("Advanced", true); //Disable all
	
})
 
var disableGroupButtons = function(from, to, id, dis) {
	for(i = from; i<= to; i++) {
		$('#' + id + "" + i).prop('disabled', dis);
	}
}

var disableSingleButtons = function(id, dis) {
		$('#' + id).prop('disabled', dis);
}