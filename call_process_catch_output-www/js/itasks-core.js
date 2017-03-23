//Test script for new itasks embedding style
itasks = {};

//Core behavior
itasks.Component = {

	domTag: 'div',
	domEl: null,

	container: true,
	containerEl: null,

	cssPrefix: 'itasks-',
	cssCls: 'component',

	width: 'flex',
	height: 'flex',
	direction: 'vertical',

	parentCmp: null,
	children: [],

	initialized: false,

	init: function() {
		var me = this;
		me.initSaplCustomization();
		me.initComponent();
		me.initChildren();
		me.renderComponent();

		me.initialized = true;
		return me;
	},
	initSaplCustomization: function() { //When necessary, apply customizatons for Check if some of the component's methods are custom defined using sapl/js
		var me = this, fun, evalfun;
		//Initialize linked sapl functions 
		if(me.saplDeps != null && me.saplDeps != '') {
			me.saplDeps = me.evalJs(me.saplDeps);
        }
		//Decode and evaluate the sapl initialization function
		if(me.saplInit !=null && me.saplInit!= '') {
			Sapl.feval([me.evalJsVal(me.saplInit),[___wrapJS(me),"JSWorld"]]);
		}
	},
	initComponent: function() {}, //Abstract method: every component implements this differently
	initChildren: function() {
		var me = this;
		me.children.forEach(function(spec,i) {
			me.beforeChildInsert(i,spec);
			me.children[i] = me.createChild(spec);
			me.children[i].init();
			me.afterChildInsert(i);
		});
	},
	renderComponent: function() {
		var me = this;
		if(me.domEl === null) { //Create a new dom element
        	me.domEl = document.createElement(me.domTag);
		} else { //Clear an existing element
			me.domEl.innerHTML = null;
		}
		//Initialially make the outer dom element also the container element that holds the child components
		me.containerEl = me.domEl;
			
		//Style the dom element
		me.domEl.classList.add(me.cssPrefix + me.cssCls);
		if(me.style) {
			me.domEl.style = me.style;
		}
		//Custom initialization after the dom element has been rendered
		me.initDOMEl();
		//Size the element
		me.initDOMElSize();
		//Set margins and alignment
		me.initDOMElMargins();
		me.initContainerEl();

		//Add the the child renderings 
		me.children.forEach(function(child) {
			me.containerEl.appendChild(child.domEl);
		});
	},
	initDOMEl: function() {},

	initDOMElSize: function() {
		var me = this,
			el = me.domEl,
			width = me.width,
			height = me.height,
			direction = (me.parentCmp && me.parentCmp.direction) || 'vertical';

		//Set width
		if(width === 'flex') {
			if(direction == 'horizontal') {
				el.style.flex = 1;
				el.style.webkitFlex = 1;
			} else {
				el.style.alignSelf = 'stretch';
				el.style.webkitAlignSelf = 'stretch';
			}
		} else if (width === 'wrap') {
			if(direction == 'horizontal') {
				el.classList.add(me.cssPrefix + 'wrapping-horizontal');
            }
        } else {
			el.style.width = width + 'px';
			el.style.minWidth = width + 'px';
			me.containerEl.style.overflowX = 'auto';
        }
		//Set height
		if(height === 'flex') {
			if(direction == 'vertical') {
				el.style.flex = 1;
				el.style.webkitFlex = 1;
			} else {
				el.style.alignSelf = 'stretch';
				el.style.webkitAlignSelf = 'stretch';
			}
		} else if (height === 'wrap') {
			if(direction == 'vertical') {
				el.classList.add(me.cssPrefix + 'wrapping-vertical');
			}
		} else {
			el.style.height = height + 'px';
			el.style.minHeight = height + 'px';
			me.containerEl.style.overflowY = 'auto';
		}
    },
	initDOMElMargins: function() {
		var me = this,
			el = me.domEl,
            width = me.width,
            height = me.height;

		if(!me.parentCmp) { //Do not set margins on the root component, let the embedding page handle that
			return;
		}
		var parentDirection = (me.parentCmp && me.parentCmp.direction) || 'vertical',
            parentVAlign = (me.parentCmp && me.parentCmp.valign) || 'top',
            parentHAlign = (me.parentCmp && me.parentCmp.halign) || 'left',
			curIdx = me.parentCmp.findChild(me),
			lastIdx = me.parentCmp.children.length - 1,
			isFirst = (curIdx == 0),
			isLast = (curIdx == lastIdx);

        //Set left and right margins as specified
		if(me.marginLeft) { el.style.marginLeft = me.marginLeft + 'px'; }
		if(me.marginRight) { el.style.marginRight = me.marginRight + 'px' ; }
	
		//Because vertical borders 'collapse' into each other, we never set the
		//bottom-margin, but set top-margin's that also include the bottom margin of
		//the previous element
		if(!isFirst) {
			//The first element never sets a top-margin. Its top-margin is added to the parent's padding
			//and its bottom margin is added to the next elements top-margin 
			el.style.marginTop = ((me.marginTop || 0) + (me.parentCmp.children[curIdx - 1].marginBottom || 0)) + 'px';
		}

		//Set margins to auto based on alignment of parent
        if(parentDirection == 'vertical') {
			if(width !== 'flex') {
				switch(parentHAlign) {
					case 'left': el.style.marginRight = 'auto'; break;
					case 'center': el.style.marginRight = 'auto'; el.style.marginLeft = 'auto'; break;
					case 'right': el.style.marginLeft = 'auto'; break;
				}
			}
            //If this element is the first, maybe also adjust top margin;
            if(curIdx === 0 && (parentVAlign == 'middle' || parentVAlign == 'bottom')) {
				el.style.marginTop = 'auto';
			}
			//If this element is the last, maybe also adjust bottom margin;
			if(curIdx === lastIdx && (parentVAlign == 'middle' || 'top')) {
				el.style.marginBottom = 'auto';
			}
		} else {
			if(height !== 'flex') {
				switch(parentVAlign) {
					case 'top': el.style.marginBottom = 'auto'; break;
					case 'middle': el.style.marginBottom = 'auto'; el.style.marginTop = 'auto'; break;
					case 'bottom': el.style.marginTop = 'auto'; break;
				}
			}
			//If this element is the first, maybe also adjust left margin;
			if(curIdx === 0 && (parentHAlign == 'center' || parentHAlign == 'right')) {
                el.style.marginLeft = 'auto';
            }
            //If this element is the last, maybe also adjust right margin;
            if(curIdx === lastIdx && (parentHAlign == 'center' || parentHAlign == 'left')) {
                el.style.marginRight = 'auto';
			}
		}
	},
	initContainerEl: function() {
		var me = this,
            el = me.containerEl,
            horizontal = (me.direction && (me.direction === 'horizontal')) || false,
			paddingTop, paddingBottom;

        el.classList.add(me.cssPrefix + (horizontal ? 'hcontainer' : 'vcontainer'));

		//Set padding
		if(me.paddingRight) { el.style.paddingRight = me.paddingRight + 'px' ; }
		if(me.paddingLeft) { el.style.paddingLeft = me.paddingLeft + 'px' ; }

		paddingTop = me.paddingTop || 0;			
		paddingBottom = me.paddingBottom || 0;
		if(me.children.length > 0) {
			paddingTop += (me.children[0].marginBottom || 0);
			paddingBottom += (me.children[me.children.length - 1].marginTop || 0);
		}
		el.style.paddingTop = paddingTop + 'px';
		el.style.paddingBottom = paddingBottom + 'px';
	},
	doEditEvent: function (taskId, editorId, value) {
		if(this.parentCmp) {
			this.parentCmp.doEditEvent(taskId, editorId, value);
		}
	},
	findChild: function(obj) {
		var me = this, num = me.children.length, i;

		for( i = 0; i < num; i++) {
			if(me.children[i] === obj) {	
				return i;
			}
		}
	},
	createChild: function(spec) {
		var me = this,
			type = spec.xtype || 'Component';
		if(itasks[type]) {
			return Object.assign(Object.create(itasks.Component),itasks[type],{parentCmp:me,children:[]},spec);
		} else {
			return Object.assign(Object.create(itasks.Component),{parentCmp:me,children:[]},spec);
		}
	},
	insertChild: function(idx = 0, spec = {}) {
		var me = this,
			child = null,
			isLast = (idx == me.children.length);
		
		me.beforeChildInsert(idx,spec);

		//Create the child object
		child = me.initialized ? me.createChild(spec) : spec ;

		//Add the child to the collection of children
		me.children.splice(idx,0,child);

		if(me.initialized) {
			//Initialize, if we are already initialized
			child.init();
			//Add the child to the dom
			if(isLast) {
				me.containerEl.appendChild(child.domEl);
			} else {
				me.containerEl.insertBefore(child.domEl,me.containerEl.childNodes[idx]);
			}
		} 
		me.afterChildInsert(idx);
	},
	beforeChildInsert: function(idx,spec) {},
	afterChildInsert: function(idx) {},
	removeChild: function(idx = 0) {
		var me = this;

		me.beforeChildRemove(idx);

		if(me.initialized) {
			me.containerEl.removeChild(me.containerEl.childNodes[idx]);
		}
		me.children.splice(idx,1);	
	},
	moveChild: function(sidx,didx) {
		var me = this, child;

		if(me.initialized) {
			if(didx == (me.containerEl.children.length - 1)) {
				me.containerEl.appendChild(me.containerEl.children[sidx]);
			} else {
				me.containerEl.insertBefore(me.containerEl.children[sidx],me.containerEl.children[(didx > sidx) ? (didx + 1) : didx]);
			}
		}

		child = me.children.splice(sidx,1)[0]; //Remove followed by insert...
		me.children.splice(didx, 0, child);
	},
	beforeChildRemove: function(idx) {},
	setAttribute: function(name,value) {
		var me = this;
	
		me[name] = value;	
		me.onAttributeChange(name,value);
	},
	onAttributeChange: function(name,value) {},
	onUIChange: function(change) {
		var me = this;
		if(change) {
			switch(change.type) {
				case 'replace':
					me.onReplaceUI(change.definition);
					break;
				case 'change':
					me.onChangeUI(change.attributes,change.children);
					break;
			}
		}
	},
	onReplaceUI: function(spec) {
		var me = this, idx;

		if(me.parentCmp) {
			idx = me.parentCmp.findChild(me);
			if(idx >= 0 ) {
				me.parentCmp.removeChild(idx);
				me.parentCmp.insertChild(idx,spec);
			}
		}
	},
	onChangeUI: function(attributeChanges,childChanges) {
		var me = this, idx;

		//Handle attribute changes
		if(attributeChanges instanceof Array) {
			attributeChanges.forEach(function(change) {
				me.setAttribute(change.name,change.value);
			});
		}
		//Handle child changes
		childChanges.forEach(function(change) {
			var idx = change[0];
			switch(change[1]) {
				case 'change':
					if(idx >= 0 && idx < me.children.length) {
						me.children[idx].onUIChange(change[2]);
					} else {
						console.log("UNKNOWN CHILD",idx,me.children.length,change);
					}
					break;
				case 'insert':
					me.insertChild(idx,change[2]);
					break;
				case 'remove':
					me.removeChild(idx);
					break;
				case 'move':
					me.moveChild(idx,change[2]);
					break;
			}
		});
	},
	onShow: function() {
		this.children.forEach(function(child) { child.onShow(); });
	},
	onHide: function() {
		this.children.forEach(function(child) { child.onHide(); });
	},
	/* Utility methods */
	evalJs: function(js) {
		var h = document.getElementsByTagName("head")[0],
			s = document.createElement("script");
		s.type = "text/javascript";
		s.appendChild(document.createTextNode(js));
		h.appendChild(s);
		h.removeChild(s);

		//Make sure that the dynamics unification is specialized for javavascript functions
		if(typeof ___SystemDynamic__unify === "function" && ___SystemDynamic__unify != _gen_unify){
			_orig_unify_fun = ___SystemDynamic__unify;
			___SystemDynamic__unify = _gen_unify;
		}
		return null;
	},
	evalJsVal: function(js) {
		var out;
 		eval("out = " + js + ";");
        return out;
	}
};
itasks.Loader = {
	cssCls: 'loader',
	initDOMEl: function() {
		var me = this,
			l = document.createElement('div');
			l.classList.add(me.cssPrefix + 'loader-spinner');
		me.domEl.appendChild(l);
	}
};
itasks.Viewport = {
	cssCls: 'viewport',
	syncTitle: false,

	initComponent:function() {
		var me = this;	

		//Use the page url as default taskUrl
		if(!me.taskUrl) {	
			me.taskUrl = '' + window.location;
		} 
		if(!me.taskUrl.endsWith('/')) {
			me.taskUrl += '/';
		}
			
		//Create a temporary root element
		me.insertChild(0,{xtype:'Loader', parentCmp: me});

		//Register the viewport with the iTasks service
		me.service = itasks.Service.getInstance();
		me.service.register(me);
	},
	getParentViewport: function() {
		var me = this, parentVp = me.parentCmp;
		while(parentVp) {
			if(parentVp.cssCls == 'viewport') { //Bit of a hack...
				return parentVp;
			}
			parentVp = parentVp.parentCmp;
		}
		return null;
	},
	doEditEvent: function (taskId, editorId, value) {
		var me = this;
		me.service.doEditEvent(taskId, editorId, value);
	},
	onInstanceUIChange: function(change) {
		var me = this;
	
		me.children[0].onUIChange(change);
		//Sync title of the top level element
		if(me.syncTitle) {
			if(change.type == 'replace' && change.definition.title) {
				document.title = change.definition.title;
			}
			if(change.type == 'change' && change.attributes.length > 0) {
				change.attributes.forEach(function(change) {
					if(change.name == 'title') {
						document.title = change.value;
					}
            	});
			}
		}
	}
};

//Convenience function for concisely creating viewports
itasks.viewport = function(spec,domEl) {
	return Object.assign(Object.create(itasks.Component), itasks.Viewport, spec, {domEl:domEl}).init();
};

//Web service proxy/multiplexer class
//This is is a singleton because all itask component objects need to share their
//connections with the server in order to limit the number of connections.

itasks.Service = {
	instances: {},
	register: function(viewport) {
		var me = this,
			taskUrl, parentViewport, taskInstance, connection;

		if(taskInstance = viewport.instanceNo) {
			//Connect to an existing task instance
			me.registerInstance_(viewport);	
			
		} else if(taskUrl = viewport.taskUrl) {
			//Create a new session
			me.createSession_(viewport);
		}
	},
	createSession_:function(viewport) {
		var me = this, connection;
		connection = me.getViewportConnection_(viewport);	
		connection.newSession(function(instanceNo,instanceKey) {
			//Store the instanceNo and key on the viewport
			viewport.instanceNo = instanceNo;
			viewport.instanceKey = instanceKey;

			//Register the instance
			me.instances[instanceNo] = {connection: connection, viewport: viewport}
			connection.attachTaskInstance(instanceNo, viewport.onInstanceUIChange.bind(viewport));
		});
	},
	registerInstance_: function(viewport) {
		var me = this, connection, instanceNo = viewport.instanceNo;
	
		connection = me.getViewportConnection_(viewport);	
		connection.attachTaskInstance(instanceNo, viewport.onInstanceUIChange.bind(viewport));

		//Register the instance
		me.instances[instanceNo] = {connection: connection, viewport: viewport}
	},
	getViewportConnection_: function(viewport) {
		var me = this, parentViewport, connection;
		//If the viewport is embedded in another viewport, reuse its connection
		if(parentViewport = viewport.getParentViewport()) {
			return me.instances[parentViewport.instanceNo].connection;
		} else {
			//Create the connection
			connection = Object.assign(Object.create(itasks.Connection),{taskUrl:viewport.taskUrl});
			connection.connect();
			return connection;
		}	
	},
	doEditEvent: function(taskId, editorId, value) {
		var me = this,
			instanceNo = taskId.split("-")[0];

		me.instances[instanceNo].connection.sendEvent(instanceNo,[taskId,editorId,value]);
	},
	unregister: function(viewport) {
	},
	getInstance: function() {
		if(typeof itasks.Service.INSTANCE === 'undefined') {
			itasks.Service.INSTANCE = Object.create(itasks.Service);
		}
		return itasks.Service.INSTANCE;
	}
};

//Abstract connection, for now a combination of an eventsource and separate requests to send events
itasks.Connection = {
	wsock: null,
	taskUrl: '',
	taskInstances: {},
	reqId: 1,
	reqCallbacks: {},
	reqDeferred: [],

	connect: function() {
		var me = this;

		me.wsock = new WebSocket('ws://' + location.host + location.pathname + (location.pathname.endsWith('/') ? '' : '/') + 'gui-wsock');
		me.wsock.onopen = function() {
			//First send deferred requests
			me.reqDeferred.forEach(function(msg) {
				me.wsock.send(msg);
			});
			//Attach currently added instances
			Object.keys(me.taskInstances).forEach(function(instanceNo) {
				me.wsock.send(JSON.stringify(["attach",parseInt(instanceNo),"dummykey"]));
			});
		};	
		me.wsock.onmessage = me.onMessage_.bind(me);
		me.wsock.onerror = me.onError_.bind(me);
	},
	newSession: function(callback) {
		var me = this, reqId = me.reqId++;
			
		me.reqCallbacks[reqId] = callback;
		if(me.wsock !== null && me.wsock.readyState == 1) {
			me.wsock.send(JSON.stringify([parseInt(reqId),"new"]));
		} else {
			me.reqDeferred.push(JSON.stringify([parseInt(reqId),"new"]));
		}
	},
	attachTaskInstance: function(taskInstance, callback) {
		var me = this;
		me.taskInstances[taskInstance] = callback;

		if(me.wsock !== null) {
			me.wsock.send(JSON.stringify(["attach",parseInt(taskInstance),"dummykey"]));
		}
	},
	removeTaskInstance: function(taskInstance) {
		delete(me.taskInstances[taskInstance]);

		if(me.wsock !== null) {
			me.wsock.send(JSON.stringify(["detach",taskInstance,"dummykey"]));
		}
	},
	sendEvent: function(taskInstance, event) {
		var me = this;

		if(me.wsock !== null) {	
			me.wsock.send(JSON.stringify(["event",parseInt(taskInstance),event]));
		}
	},
	disconnect: function() {
		var me = this;
		if(me.wsock !== null) {
			me.wsock.close();
			me.wsock = null;
		}
	},
	onError_: function(e) {
		console.log("ERROR",e);
	},
	onReset_: function(e) {
		console.log("ERROR",e);
	},
	onMessage_: function(e) {
		var me = this,
			msg = JSON.parse(e.data);
		//Check if it is a response to request
		if(msg instanceof Array) {
			var reqId = msg[0],
				reqRsp = msg[1];
			if(me.reqCallbacks[reqId]) {
				me.reqCallbacks[reqId](reqRsp.instanceNo,reqRsp.instanceKey);
			}
		//UI synchronization
		} else {
			var taskInstance = msg.instance,
				change = msg.change;

			if(me.taskInstances[taskInstance]) {
				me.taskInstances[taskInstance](change);
			} 
		}
	}
};

