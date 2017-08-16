itasks.RawEmpty = {
	cssCls: 'raw-empty',
	initDOMEl: function() {
		this.domEl.innerHTML = '(empty)';
	}
};
itasks.RawRecord = {
	cssCls: 'raw-record'
};
itasks.RawCons = {
	cssCls: 'raw-cons'
};
itasks.RawVarCons = {
	cssCls: 'raw-var-cons'
};
itasks.RawInteract = {
	cssCls: 'raw-interact'
};
itasks.RawStep = {
	cssCls: 'raw-step',	
};
itasks.RawParallel = {
	cssCls: 'raw-parallel',	
};
itasks.RawAction = {
	cssCls: 'raw-action',	
	domTag: 'a',
	width: 'wrap',
	initDOMEl: function() {
		var me = this, el = me.domEl;

		el.innerHTML = me.actionId;
		el.href = '#';
		el.classList.add(this.cssPrefix + (me.enabled ? 'raw-action-enabled' : 'raw-action-disabled'));
		el.addEventListener('click',function(e) {
			me.doEditEvent(me.taskId,null,me.actionId);
			e.preventDefault();
		});
    },
	onAttributeChange: function(name,value) {
		var me = this, el = me.domEl;
		switch(name) {
			case 'enabled':
				me.enabled = value;
				if(me.enabled) {
					el.classList.remove(this.cssPrefix + 'raw-action-disabled');
					el.classList.add(this.cssPrefix + 'raw-action-enabled');
				} else {
					el.classList.remove(this.cssPrefix +'raw-action-enabled');
					el.classList.add(this.cssPrefix +'raw-action-disabled');
				}
				break;
		}
	}
};
