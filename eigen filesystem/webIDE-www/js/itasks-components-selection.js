//Mixin containing the selection/toggle behavior 
itasks.Selector = { 
	select: function (selection, toggle = false) {
        var me = this,
			options = me.options,
			oldSelection = me.value.slice(0),
			i;
		if(toggle) {
 			//Unselect items in the toggle set
			me.value = me.value.filter(function(x) {return !selection.includes(x)});
			//Add the items from the selection that were not already selected
			me.value = me.value.concat(selection.filter(function(x) {return !oldSelection.includes(x)}));
		} else {
			me.value = selection;
		}
		//Update DOM
		options.forEach(me.selectOptionsInDOM.bind(me));
	},
	selectOptionsInDOM: function(option) {
		var me = this;
		me.selectInDOM(option.domEl,me.value.includes(option.id));
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
    width: 150,
	multiple: false,

    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            optionEl;

		//The empty selection
        optionEl = document.createElement('option');
        optionEl.innerHTML = "Select...";
        optionEl.value = "";
        el.appendChild(optionEl);

        me.options.forEach(function(option) {

            optionEl = document.createElement('option');
            optionEl.value = option.id;
            optionEl.innerHTML = option.text;
            if(me.value.includes(option.id)) {
                optionEl.selected = true;
            }
            el.appendChild(optionEl);
			option.domEl = optionEl;
        },me);

        el.addEventListener('change',function(e) {
			me.select(e.target.value === '' ? [] : [parseInt(e.target.value)]);
            me.doEditEvent(me.taskId,me.editorId,me.value);
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
	multiple: false,
	initDOMEl: function() {
		var me = this,
			el = me.domEl,
			inputName = "choice-" + me.taskId + "-" + me.editorId;

		me.options.forEach(function(option,idx) {
			var liEl,inputEl,labelEl;
			liEl = document.createElement('li');
			inputEl = document.createElement('input');
			inputEl.type = me.multiple ? 'checkbox' : 'radio';
			inputEl.value = idx;
			inputEl.name = inputName;
			inputEl.id = inputName + "-option-" + option.id;
			if(me.value.includes(option.id)) {
				inputEl.checked = true;
            }
            inputEl.addEventListener('click',function(e) {
				me.select([option.id],me.multiple);
				me.doEditEvent(me.taskId,me.editorId,me.value);
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
	multiple: false,
    initDOMEl: function() {
        var me = this,
            el = me.domEl;

        me.options.forEach(function(option,idx) {
            var optionEl;
            optionEl = document.createElement('div');
            optionEl.classList.add(me.cssPrefix + 'choice-list-option');

            optionEl.addEventListener('click',function(e) {
				me.select([option.id], me.multiple && (e.metaKey || e.ctrlKey));
                me.doEditEvent(me.taskId,me.editorId,me.value);
				e.preventDefault();
            });
            optionEl.innerHTML = option.text;

            el.appendChild(optionEl);
			option.domEl = optionEl;
        });
		me.optionsDOM = me.domEl.children;
    },
	setOptions: function(options) {
	},
	selectInDOM(el,selected) {
		el.classList[selected ? 'add':'remove'](this.cssPrefix + 'selected');
	}
},itasks.Selector);

itasks.Grid = Object.assign({
	cssCls: 'choicegrid',
	width: 'flex',
	height: 'flex',
	multiple: false,

    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            headerEl,bodyEl,rowEl,cellEl;

        //Create header
        headerEl = me.headerEl = document.createElement('div');
        headerEl.classList.add(me.cssPrefix + 'choicegrid-header');
        me.columns.forEach(function(column) {
            cellEl = document.createElement('div');
            cellEl.innerHTML = column;
            headerEl.appendChild(cellEl);
        });
        el.appendChild(headerEl);

        //Create body
        bodyEl = me.bodyEl = document.createElement('div');
        bodyEl.classList.add(me.cssPrefix + 'choicegrid-body');

		//Fill with options
		me.setOptions(me.options);
/*
        me.options.forEach(function(option,rowIdx) {
            rowEl = document.createElement('div');
            rowEl.addEventListener('click',function(e) {
				me.select([option.id], me.multiple && (e.metaKey || e.ctrlKey));
                me.doEditEvent(me.taskId,me.editorId,me.value);
            },me);
            if(me.doubleClickAction) {
                rowEl.addEventListener('dblclick',function(e) {
					me.select([option.id]);
                    me.doEditEvent(me.taskId,me.editorId,me.value);
                    me.sendActionEvent(me.doubleClickAction[0],me.doubleClickAction[1]);

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
*/
        //Indicate initial selection
        if(me.value.length) {
            me.value.forEach(function(selectedIdx) {
                bodyEl.childNodes[selectedIdx].classList.add(me.cssPrefix + 'selected');
            });
        }
        el.appendChild(bodyEl);
    },
	setOptions: function(options) {
		var me = this, bodyEl = me.bodyEl;
		//Store options
		me.options = options;

		//Clear
		while (bodyEl.lastChild) {
    		bodyEl.removeChild(bodyEl.lastChild);
		}
		//Add rows
        options.forEach(function(option,rowIdx) {
            rowEl = document.createElement('div');
            rowEl.addEventListener('click',function(e) {
				me.select([option.id], me.multiple && (e.metaKey || e.ctrlKey));
                me.doEditEvent(me.taskId,me.editorId,me.value);
            },me);
            if(me.doubleClickAction) {
                rowEl.addEventListener('dblclick',function(e) {
					me.select([option.id]);
                    me.doEditEvent(me.taskId,me.editorId,me.value);
                    me.sendActionEvent(me.doubleClickAction[0],me.doubleClickAction[1]);

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
    height: 'flex',
	multiple: false,
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            rootNodeId = me.taskId+ "-" + me.editorId + "-node",
            rootNode,node;

        el.classList.add(me.cssPrefix + 'choicetree');

        rootNode = document.createElement('ol');

        me.options.forEach(function(option,idx) {
            me.addNode(option,rootNode,rootNodeId,idx);
        },me);

        me.value.forEach(function(idx) {
            me.nodes[idx].classList.add(me.cssPrefix + 'selected');
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
				me.select([option.id],me.multiple && (e.metaKey || e.ctrlKey));
                me.doEditEvent(me.taskId,me.editorId,me.value);
        },me);

        if(me.doubleClickAction) {
            label.addEventListener('dblclick',function(e) {
				me.select([option.id]);
                me.doEditEvent(me.taskId,me.editorId,me.value);
                me.doEditEvent(me.doubleClickAction[0],null,me.doubleClickAction[1]);

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
