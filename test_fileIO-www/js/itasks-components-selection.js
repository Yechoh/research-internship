//Mixin containing the selection/toggle behavior 
itasks.Selector = { 
	select: function (selection, toggle = false) {
        var me = this,
			options = me.attributes.options,
			oldSelection = me.attributes.value.slice(0),
			i;
		if(toggle) {
 			//Unselect items in the toggle set
			me.attributes.value = me.attributes.value.filter(function(x) {return !selection.includes(x)});
			//Add the items from the selection that were not already selected
			me.attributes.value = me.attributes.value.concat(selection.filter(function(x) {return !oldSelection.includes(x)}));
		} else {
			me.attributes.value = selection;
		}
		//Update DOM
		options.forEach(me.selectOptionsInDOM.bind(me));
	},
	selectOptionsInDOM: function(option) {
		var me = this;
		me.selectInDOM(option.domEl,me.attributes.value.includes(option.id));
		if(option.children) {
			option.children.forEach(me.selectOptionsInDOM.bind(me));
		}
	},
	selectInDOM(el,selected) {
		el.classList[selected ? 'add':'remove'](this.cssPrefix + 'selected');
	},
    onAttributeChange: function(name,value) {
		var me = this;
		switch(name) {
			case 'value':
			me.select(value,false);
			break;
			case 'options':
			me.setOptions(value);
			break;
		}
	}
};

itasks.Dropdown = Object.assign({
    domTag: 'select',
	attributes: {
    	width: 150,
		height: 'wrap',
		multiple: false
	},
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            optionEl;

		//The empty selection
        optionEl = document.createElement('option');
        optionEl.innerHTML = "Select...";
        optionEl.value = "";
        el.appendChild(optionEl);

        me.attributes.options.forEach(function(option) {

            optionEl = document.createElement('option');
            optionEl.value = option.id;
            optionEl.innerHTML = option.text;
            if(me.attributes.value.includes(option.id)) {
                optionEl.selected = true;
            }
            el.appendChild(optionEl);
			option.domEl = optionEl;
        },me);

        el.addEventListener('change',function(e) {
			me.select(e.target.value === '' ? [] : [parseInt(e.target.value)]);
            me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
        });
    },
	selectInDOM: function(el,selected) {
		el.selected = selected;
	},
	setOptions: function(options) {
		
	}
},itasks.Selector);

itasks.CheckGroup = Object.assign({
	domTag: 'ul',
	cssCls: 'checkgroup',
	attributes: {
		multiple: false,
	},
	initDOMEl: function() {
		var me = this,
			el = me.domEl,
			inputName = "choice-" + me.attributes.taskId + "-" + me.attributes.editorId;

		me.attributes.options.forEach(function(option,idx) {
			var liEl,inputEl,labelEl;
			liEl = document.createElement('li');
			inputEl = document.createElement('input');
			inputEl.type = me.attributes.multiple ? 'checkbox' : 'radio';
			inputEl.value = idx;
			inputEl.name = inputName;
			inputEl.id = inputName + "-option-" + option.id;
			if(me.attributes.value.includes(option.id)) {
				inputEl.checked = true;
            }
            inputEl.addEventListener('click',function(e) {
				me.select([option.id],me.attributes.multiple);
				me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
            });
			liEl.appendChild(inputEl);

			labelEl = document.createElement('label');
			labelEl.setAttribute('for',inputName + "-option-" + option.id);
			labelEl.innerHTML = option.text;
			liEl.appendChild(labelEl);

            el.appendChild(liEl);
			option.domEl = liEl;
        });
		me.optionsDOM = me.domEl.children;
    },
	setOptions: function(options) {
		
	},
	selectInDOM: function(el,selected) {
		el.children[0].checked = selected;
	}
},itasks.Selector);

itasks.ChoiceList = Object.assign({
	cssCls: 'choice-list',
	attributes: {
		multiple: false
	},
    initDOMEl: function() {
        var me = this,
            el = me.domEl;

        me.attributes.options.forEach(function(option,idx) {
            var optionEl;
            optionEl = document.createElement('div');
            optionEl.classList.add(me.cssPrefix + 'choice-list-option');

            optionEl.addEventListener('click',function(e) {
				me.select([option.id], me.attributes.multiple && (e.metaKey || e.ctrlKey));
                me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
				e.preventDefault();
            });
            optionEl.innerHTML = option.text;

            el.appendChild(optionEl);
			option.domEl = optionEl;
        });
		me.optionsDOM = me.domEl.children;

		//Highlight initital selection
		me.select(me.attributes.value,false);
    },
	setOptions: function(options) {
	},
	selectInDOM(el,selected) {
		el.classList[selected ? 'add':'remove'](this.cssPrefix + 'selected');
	}
},itasks.Selector);

itasks.Grid = Object.assign({
	cssCls: 'choicegrid',
	attributes: {
		width: 'flex',
		height: 'flex',
		multiple: false
	},
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            headerEl,bodyEl,rowEl,cellEl;

        //Create header
        headerEl = me.headerEl = document.createElement('div');
        headerEl.classList.add(me.cssPrefix + 'choicegrid-header');
        me.attributes.columns.forEach(function(column) {
            cellEl = document.createElement('div');
            cellEl.innerHTML = column;
            headerEl.appendChild(cellEl);
        });
        el.appendChild(headerEl);

        //Create body
        bodyEl = me.bodyEl = document.createElement('div');
        bodyEl.classList.add(me.cssPrefix + 'choicegrid-body');

		//Fill with options
		me.setOptions(me.attributes.options);

        //Indicate initial selection
        if(me.attributes.value.length) {
            me.attributes.value.forEach(function(selectedIdx) {
                bodyEl.childNodes[selectedIdx].classList.add(me.cssPrefix + 'selected');
            });
        }
        el.appendChild(bodyEl);
    },
	setOptions: function(options) {
		var me = this, bodyEl = me.bodyEl;
		//Store options
		me.attributes.options = options;

		//Clear
		while (bodyEl.lastChild) {
    		bodyEl.removeChild(bodyEl.lastChild);
		}
		//Add rows
        options.forEach(function(option,rowIdx) {
            rowEl = document.createElement('div');
            rowEl.addEventListener('click',function(e) {
				me.select([option.id], me.attributes.multiple && (e.metaKey || e.ctrlKey));
                me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
            },me);
            if(me.attributes.doubleClickAction) {
                rowEl.addEventListener('dblclick',function(e) {
					me.select([option.id]);
                    me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
                    me.sendActionEvent(me.attributes.doubleClickAction[0],me.attributes.doubleClickAction[1]);

                    e.stopPropagation();
                    e.preventDefault();
                },me);
            }
            option.cells.forEach(function(cell) {
                cellEl = document.createElement('div');
                cellEl.innerHTML = cell;
                rowEl.appendChild(cellEl);
            });
            bodyEl.appendChild(rowEl);
			option.domEl = rowEl;
        });
	},
	initContainerEl: function() {},
	selectInDOM(el,selected) {
		el.classList[selected ? 'add':'remove'](this.cssPrefix + 'selected');
	}
},itasks.Selector);

itasks.Tree = Object.assign({
	attributes: {
    	height: 'flex',
		multiple: false,
		options: []
	},
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            rootNodeId = me.attributes.taskId + "-" + me.attributes.editorId + "-node",
            rootNode,node;

        el.classList.add(me.cssPrefix + 'choicetree');

        rootNode = document.createElement('ol');

        me.attributes.options.forEach(function(option,idx) {
            me.addNode(option,rootNode,rootNodeId,idx);
        },me);

        me.attributes.value.forEach(function(idx) {
			if(me.attributes.options && me.attributes.options[idx]) {
				me.attributes.options[idx].domEl.classList.add(me.cssPrefix + 'selected');
			}
        });
        el.appendChild(rootNode);
    },
    addNode: function(option,parentNode,rootNodeId,idx) {
        var me = this,
            node,nodeId,label,childExpand,childOl;

        nodeId = rootNodeId + "-"+ idx;
        node = document.createElement('li');
        node.id = nodeId;

        label = document.createElement('label');
        label.id = nodeId + "-l";

        if(option.iconCls) {
            label.classList.add(option.iconCls);
        } else {
            label.classList.add(me.cssPrefix + 'default-' + (option.children.length ? 'folder' : 'leaf'));
        }
        label.innerHTML = option.text;
        label.addEventListener('click',function(e) {
				me.select([option.id],me.attributes.multiple && (e.metaKey || e.ctrlKey));
                me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
        },me);

        if(me.attributes.doubleClickAction) {
            label.addEventListener('dblclick',function(e) {
				me.select([option.id]);
                me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
                me.doEditEvent(me.attributes.doubleClickAction[0],null,me.attributes.doubleClickAction[1]);

                e.stopPropagation();
                e.preventDefault();
            });
        }
        node.appendChild(label);

        if(option.children && option.children.length) {
            childExpand = document.createElement('input');
            childExpand.type = "checkbox"
            childExpand.id = nodeId + "-e";

            if(option.expanded) {
                childExpand.checked = true;
            }
            childExpand.addEventListener('click',function(e) {
                //me.doEditEvent(me.taskId,me.editorId,["exp",option.value,childExpand.checked]);
            },me);

            node.appendChild(childExpand);
            childOl = document.createElement('ol');
            option.children.forEach(function(option,childIdx) {
                me.addNode(option,childOl,nodeId,childIdx);
            },me);
            node.appendChild(childOl);
        }
        parentNode.appendChild(node);

        //Track the option in the dom
		option.domEl = node;
    },
	setOptions: function(options) {
		
	}
},itasks.Selector);
