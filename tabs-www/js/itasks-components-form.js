itasks.TextField = {
	domTag: 'input',
	initDOMEl: function() {
		var me = this,
			el = this.domEl;
		el.type = 'text';
		el.value = me.value ? me.value : '';
		el.addEventListener('keyup',function(e) {
            var value = e.target.value === "" ? null : e.target.value
			me.doEditEvent(me.taskId,me.editorId,value);
		});
	},
	onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
			if(me.domEl !== document.activeElement || ! document.hasFocus() ) { //Don't update the focused element...
				me.domEl.value = (value === null) ? '' : value;
			}
		}
	}
};
itasks.TextArea = {
    domTag: 'textarea',
	height: 150,
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.innerHTML = me.value ? me.value : '';
        el.addEventListener('keyup',function(e) {
			var value = e.target.value === "" ? null : e.target.value
			me.doEditEvent(me.taskId,me.editorId,value);
        });
    },
	onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
			if(me.domEl !== document.activeElement || ! document.hasFocus()) { //Don't update the focused element...
				me.domEl.value = (value === null) ? '' : value;
			}
		}
	}
};
itasks.PasswordField = {
	domTag: 'input',
	initDOMEl: function() {
		var me = this,
			el = this.domEl;
		el.type = 'password';
		el.value = me.value ? me.value : '';
		el.addEventListener('keyup',function(e) {
            var value = e.target.value === "" ? null : e.target.value
			me.doEditEvent(me.taskId,me.editorId,value);
		});
	}
	,onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
			if(me.domEl !== document.activeElement || ! document.hasFocus()) { //Don't update the focused element...
				me.domEl.value = (value === null) ? '' : value;
			}
		}
	}
};
itasks.NumberField = {
	domTag: 'input',
    allowDecimal: false,
	width: 150,
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'text';
		el.value = (me.value === undefined || me.value === null) ? '' : me.value;

        el.addEventListener('keypress',function(e) {
            if(me.invalidKey(e.which)) {
                e.stopPropagation();
                e.preventDefault();
            }
        });
        el.addEventListener('keyup',function(e) {
            var value;
            if(me.invalidKey(e.which)) {
                return;
            }
            if(e.target.value === "") {
                value = null;
            } else if(me.invalidValue(e.target.value)) {
                value = e.target.value;
            } else {
                value = me.allowDecimal ? parseFloat(e.target.value) : (e.target.value | 0);
            }
            me.doEditEvent(me.taskId,me.editorId,value);
        });
    },
    invalidKey: function(charCode) {
        return !(charCode < 32 || (charCode > 47 && charCode < 58) || charCode == 45 || (this.allowDecimal && charCode == 46));
    },
    invalidValue: function(value) {
        var me = this, i;
        for(i = 0; i < value.length; i++) {
            if(me.invalidKey(value.charCodeAt(i))) {
                return true;
            }
        }
        return false;
    },
	onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
			if(me.domEl !== document.activeElement || ! document.hasFocus()) { //Don't update the focused element...
				me.domEl.value = (value === null) ? '' : value;
			}
		}
	}
};
itasks.IntegerField = Object.assign(itasks.NumberField,{
    allowDecimal: false
});
itasks.DecimalField = Object.assign(itasks.NumberField,{
    allowDecimal: true
});
itasks.DocumentField = {
	cssCls: 'edit-document',
    initDOMEl: function() {

        var me = this,
            el = this.domEl;

        //Create a hidden file selector
        me.fileEl = document.createElement('input');
        me.fileEl.type = "file";
        me.fileEl.style.display = "none";
        me.fileEl.addEventListener('change',me.onFileSelect.bind(me));
        el.appendChild(me.fileEl);

        me.labelEl = document.createElement('span');
        el.appendChild(me.labelEl);

        me.actionEl = document.createElement('a');
        me.actionEl.href = "#";
        me.actionEl.addEventListener('click',me.onAction.bind(me));
        el.appendChild(me.actionEl);

        me.xhr = null;
        me.value = me.value || null;
        me.showValue();
    },
    showUploading: function(progress) {
        this.labelEl.innerHTML = "Uploading... " + progress + "%";
        this.actionEl.innerHTML = "Cancel";
    },
    showValue: function() {
        var me = this;
        if(me.value !== null) {
            me.labelEl.innerHTML = '<a href="'+me.value.contentUrl+'" target="_blank">'+me.value.name+'</a>';
            me.actionEl.innerHTML = 'Clear';
        } else {
            me.labelEl.innerHTML = 'No file selected';
            me.actionEl.innerHTML = 'Select';
        }
    },
    onAction: function(e) {
        var me = this;
        e.preventDefault();

        if(me.xhr != null) { //Cancel
            me.xhr.abort();
            me.xhr = null;
            me.showValue();
            return;
        }
        if(me.value != null) { //Clear;
            me.doEditEvent(me.taskId,me.editorId,null);
            me.value = null;
            me.showValue();
        } else { //Select
            me.fileEl.click();
        }
    },
    onFileSelect: function() {
        var me = this,
            fd;

        //Create uploader
        me.xhr = new XMLHttpRequest();
        me.xhr.upload.addEventListener('progress',function(e) {
            me.showUploading(Math.round((e.loaded * 100) / e.total));
        });
        me.xhr.onreadystatechange = me.onUploadStateChange.bind(me);
        me.xhr.open('POST','/upload',true);
        //Add file to upload data
        fd = new FormData();
        fd.append('upload',me.fileEl.files[0]);
        me.xhr.send(fd);
    },
    onUploadStateChange: function(e) {
        var me = this, rsp;

        if (me.xhr.readyState == 4 && me.xhr.status == 200) {
            //Upload ready
            rsp = JSON.parse(me.xhr.responseText);

            //Switch to value state
            me.doEditEvent(me.taskId,me.editorId,rsp[0]);
            me.xhr = null;
            me.value = rsp[0];
            me.showValue();
        }
    },
    setEditorValue: function(value) {
        if(me.xhr != null) {
            me.xhr.abort();
            me.xhr = null;
        }
        me.value = value; 
        me.showValue();
    }
};
itasks.Checkbox = {
	domTag: 'input',
    width: 'wrap',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'checkbox';
        el.checked = me.value;

        el.addEventListener('click',function(e) {
			var value = e.target.checked;
            me.doEditEvent(me.taskId,me.editorId,value);
        });
    },
	onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
        	me.domEl.checked = value;
		}
	}
};
itasks.Slider = {
	domTag: 'input',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'range';
        el.min = me.min;
        el.max = me.max;
        el.value = me.value;

        el.addEventListener('change',function(e) {
            me.doEditEvent(me.taskId,me.editorId, (e.target.value | 0),true);
        });
    },
	onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
        	me.domEl.value = value;
		}
	}
};
itasks.Button = {
	domTag: 'a',
	cssCls: 'button',
	height: 'wrap',
	width: 'wrap',
	enabled: true,

	initDOMEl: function() {
		var me = this,
			el = me.domEl;

		el.href = '#';
		if(me.iconCls) {
			me.icon = document.createElement('div');
			me.icon.classList.add(me.cssPrefix + 'button-icon');
			me.icon.classList.add(me.iconCls);
			el.appendChild(me.icon);
		}
		if(!me.enabled) {
			el.classList.add(me.cssPrefix + 'button-disabled');
		}
		if(me.text) {
			me.label = document.createElement('div');
			me.label.innerHTML = me.text;
			me.label.classList.add(me.cssPrefix + 'button-label');
			el.appendChild(me.label);
		}
        el.addEventListener('click',function(e) {
			if(typeof(me.value) == 'boolean') { //Toggle edit buttons
				me.value = !me.value;
			}
            if(me.enabled) {
				me.doEditEvent(me.taskId,me.editorId,me.value);
            }
			e.preventDefault();
			return false;
		});
    },
	initContainerEl: function() { //Make sure no padding is set on buttons
	},
	onAttributeChange: function(name,value) {
		var me = this;
		switch(name) {
			case 'enabled':
				me.enabled = value;
				if(value) {
					me.domEl.classList.remove(me.cssPrefix + 'button-disabled');
				} else {
					me.domEl.classList.add(me.cssPrefix + 'button-disabled');
				}
				break;
		}
	}

};
itasks.Label = {
    domTag: 'label',
    initDOMEl: function() {
        var me = this,
            el = me.domEl;
        el.innerHTML = me.text;
    }
};
itasks.Icon = {

	width: 'wrap',
	height: 'wrap',
	initDOMEl: function() {
		var me = this,
			el = me.domEl;
		el.classList.add(me.cssPrefix + 'icon');
		el.classList.add(me.iconCls);
		me.currentIcon = me.iconCls;

		if(me.tooltip) {
			el.setAttribute('tooltip',me.tooltip);
		}
    },
	onAttributeChange: function(name,value) {
		var me = this,
			el = me.domEl;
		switch(name) {
			case 'iconCls':
				el.classList.remove(me.currentIcon);
				me.currentIcon = value;
				el.classList.add(me.currentIcon);
				break;
			case 'tooltip':
				el.setAttribute('tooltip',value);
				break;
		}
	}
};

