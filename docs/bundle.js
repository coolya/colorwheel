!function(t){function r(e){if(n[e])return n[e].exports;var a=n[e]={i:e,l:!1,exports:{}};return t[e].call(a.exports,a,a.exports,r),a.l=!0,a.exports}var n={};r.m=t,r.c=n,r.d=function(t,n,e){r.o(t,n)||Object.defineProperty(t,n,{configurable:!1,enumerable:!0,get:e})},r.n=function(t){var n=t&&t.__esModule?function(){return t.default}:function(){return t};return r.d(n,"a",n),n},r.o=function(t,r){return Object.prototype.hasOwnProperty.call(t,r)},r.p="",r(r.s=9)}([function(t,r,n){"use strict";function e(t,r){if(!(t instanceof r))throw new TypeError("Cannot call a class as a function")}function a(t,r){if("System.Collections.Generic.IEnumerable"===r)return"function"==typeof t[Symbol.iterator];if("function"==typeof t[h.a.reflection]){var n=t[h.a.reflection]().interfaces;return Array.isArray(n)&&n.indexOf(r)>-1}return!1}function u(t){if(null==t)return[];var r="function"==typeof t[h.a.reflection]?t[h.a.reflection]().properties||[]:t;return Object.getOwnPropertyNames(r)}function i(t){function r(t){return!(null===t||"object"!==(void 0===t?"undefined":g(t))||t instanceof Number||t instanceof String||t instanceof Boolean)}var n=arguments.length>1&&void 0!==arguments[1]&&arguments[1];if(null==t||"number"==typeof t)return String(t);if("string"==typeof t)return n?JSON.stringify(t):t;if(t instanceof Date)return Object(b.b)(t);if("function"==typeof t.ToString)return t.ToString();if(a(t,"FSharpUnion")){var e=t[h.a.reflection](),u=e.cases[t.tag];switch(u.length){case 1:return u[0];case 2:return u[0]+" ("+i(t.data,!0)+")";default:return u[0]+" ("+t.data.map(function(t){return i(t,!0)}).join(",")+")"}}try{return JSON.stringify(t,function(t,n){return n&&n[Symbol.iterator]&&!Array.isArray(n)&&r(n)?Array.from(n):n&&"function"==typeof n.ToString?i(n):n})}catch(r){return"{"+Object.getOwnPropertyNames(t).map(function(r){return r+": "+String(t[r])}).join(", ")+"}"}}function o(t){if(null!=t&&"function"==typeof t.GetHashCode)return t.GetHashCode();for(var r=i(t),n=5381,e=0,a=r.length;e<a;)n=33*n^r.charCodeAt(e++);return n}function f(t,r){if(t===r)return!0;if(null==t)return null==r;if(null==r)return!1;if("function"==typeof t.Equals)return t.Equals(r);if("function"==typeof r.Equals)return r.Equals(t);if(Object.getPrototypeOf(t)!==Object.getPrototypeOf(r))return!1;if(Array.isArray(t)){if(t.length!==r.length)return!1;for(var n=0;n<t.length;n++)if(!f(t[n],r[n]))return!1;return!0}if(ArrayBuffer.isView(t)){if(t.byteLength!==r.byteLength)return!1;for(var e=new DataView(t.buffer),a=new DataView(r.buffer),u=0;u<t.byteLength;u++)if(e.getUint8(u)!==a.getUint8(u))return!1;return!0}return t instanceof Date&&t.getTime()===r.getTime()}function c(t,r){if(t===r)return 0;if(null==t)return null==r?0:-1;if(null==r)return 1;if("function"==typeof t.CompareTo)return t.CompareTo(r);if("function"==typeof r.CompareTo)return-1*r.CompareTo(t);if(Object.getPrototypeOf(t)!==Object.getPrototypeOf(r))return-1;if(Array.isArray(t)){if(t.length!==r.length)return t.length<r.length?-1:1;for(var n=0,e=0;n<t.length;n++)if(0!==(e=c(t[n],r[n])))return e;return 0}if(ArrayBuffer.isView(t)){if(t.byteLength!==r.byteLength)return t.byteLength<r.byteLength?-1:1;for(var a=new DataView(t.buffer),u=new DataView(r.buffer),i=0,l=0,s=0;i<t.byteLength;i++){if(l=a.getUint8(i),s=u.getUint8(i),l<s)return-1;if(l>s)return 1}return 0}if(t instanceof Date)return Object(b.a)(t,r);if("object"===(void 0===t?"undefined":g(t))){var d=o(t),v=o(r);return d===v?f(t,r)?0:-1:d<v?-1:1}return t<r?-1:1}function l(t,r){if(t===r)return!0;var n=u(t),e=!0,a=!1,i=void 0;try{for(var o,c=n[Symbol.iterator]();!(e=(o=c.next()).done);e=!0){var l=o.value;if(!f(t[l],r[l]))return!1}}catch(t){a=!0,i=t}finally{try{!e&&c.return&&c.return()}finally{if(a)throw i}}return!0}function s(t,r){if(t===r)return 0;var n=t.tag<r.tag?-1:t.tag>r.tag?1:0;return 0!==n?n:c(t.data,r.data)}function d(t){var r=t;return function(){return 0===arguments.length?r:void(r=arguments[0])}}function v(t){var r=arguments.length>1&&void 0!==arguments[1]?arguments[1]:0,n=Math.pow(10,r),e=+(r?t*n:t).toFixed(8),a=Math.floor(e),u=e-a,i=u>.5-1e-8&&u<.5+1e-8?a%2==0?a:a+1:Math.round(e);return r?i/n:i}r.f=i,r.d=f,r.a=c,r.b=s,r.c=d,r.e=v;var b=n(5),h=n(2),g="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t},y=function(){function t(t,r){for(var n=0;n<r.length;n++){var e=r[n];e.enumerable=e.enumerable||!1,e.configurable=!0,"value"in e&&(e.writable=!0),Object.defineProperty(t,e.key,e)}}return function(r,n,e){return n&&t(r.prototype,n),e&&t(r,e),r}}(),p=function(){function t(r,n,a){e(this,t),this.kind=r,this.definition=n,this.generics=a}return y(t,[{key:"Equals",value:function(t){return this.kind===t.kind&&this.definition===t.definition&&("object"===g(this.generics)?l(this.generics,t.generics):this.generics===t.generics)}}]),t}();new p("Any"),new p("Unit")},function(t,r,n){"use strict";function e(t,r,n){return r in t?Object.defineProperty(t,r,{value:n,enumerable:!0,configurable:!0,writable:!0}):t[r]=n,t}function a(t,r){if(!(t instanceof r))throw new TypeError("Cannot call a class as a function")}function u(t){if(null==t)throw new Error("Seq did not contain any matching element");return Object(S.b)(t)}function i(t){return l(function(t,r){return new k.a(t,r)},t,new k.a)}function o(t,r,n){var e=y(function(t){return 0!==t},g(function(r,n){return t(r,n)},r,n));return null!=e?Object(S.b)(e):b(r)-b(n)}function f(t){return e({},Symbol.iterator,function(){return t()[Symbol.iterator]()})}function c(t,r,n){if(Array.isArray(n)||ArrayBuffer.isView(n))return n.reduce(t,r);for(var e=void 0,a=0,u=n[Symbol.iterator]();e=u.next(),!e.done;a++)r=t(r,e.value,a);return r}function l(t,r,n){for(var e=Array.isArray(r)||ArrayBuffer.isView(r)?r:Array.from(r),a=e.length-1;a>=0;a--)n=t(e[a],n,a);return n}function s(t,r){if(t<0)return null;if(Array.isArray(r)||ArrayBuffer.isView(r))return t<r.length?new S.a(r[t]):null;for(var n=0,e=r[Symbol.iterator]();;n++){var a=e.next();if(a.done)break;if(n===t)return new S.a(a.value)}return null}function d(t,r){return u(s(t,r))}function v(t,r){c(function(r,n){return t(n)},null,r)}function b(t){return Array.isArray(t)||ArrayBuffer.isView(t)?t.length:c(function(t,r){return t+1},0,t)}function h(t,r){return f(function(){return m(function(r){var n=r.next();return n.done?null:[t(n.value),r]},r[Symbol.iterator]())})}function g(t,r,n){return f(function(){var e=r[Symbol.iterator](),a=n[Symbol.iterator]();return m(function(){var r=e.next(),n=a.next();return r.done||n.done?null:[t(r.value,n.value),null]})})}function y(t,r,n){for(var e=0,a=r[Symbol.iterator]();;e++){var u=a.next();if(u.done)break;if(t(u.value,e))return new S.a(u.value)}return void 0===n?null:new S.a(n)}function p(t,r){for(var n=0,e=r[Symbol.iterator]();;n++){var a=e.next();if(a.done)break;var u=t(a.value,n);if(null!=u)return u}return null}function w(t,r){return u(p(t,r))}function m(t,r){return e({},Symbol.iterator,function(){var n=r;return{next:function(){var r=t(n);return null!=r?(n=r[1],{done:!1,value:r[0]}):{done:!0}}}})}function O(t,r){return g(function(t,r){return[t,r]},t,r)}r.h=i,r.a=o,r.b=c,r.c=l,r.d=d,r.e=v,r.f=h,r.i=p,r.g=w,r.j=O;var k=(n(7),n(3)),S=n(4),j=(n(0),function(){function t(t,r){var n=[],e=!0,a=!1,u=void 0;try{for(var i,o=t[Symbol.iterator]();!(e=(i=o.next()).done)&&(n.push(i.value),!r||n.length!==r);e=!0);}catch(t){a=!0,u=t}finally{try{!e&&o.return&&o.return()}finally{if(a)throw u}}return n}}(),function(){function t(t,r){for(var n=0;n<r.length;n++){var e=r[n];e.enumerable=e.enumerable||!1,e.configurable=!0,"value"in e&&(e.writable=!0),Object.defineProperty(t,e.key,e)}}return function(r,n,e){return n&&t(r.prototype,n),e&&t(r,e),r}}());!function(){function t(r){a(this,t),this.iter=r}j(t,[{key:"MoveNext",value:function(){var t=this.iter.next();return this.current=t.value,!t.done}},{key:"Reset",value:function(){throw new Error("JS iterators cannot be reset")}},{key:"Dispose",value:function(){}},{key:"Current",get:function(){return this.current}},{key:"get_Current",get:function(){return this.current}}])}()},function(t,r,n){"use strict";function e(t,r){a.set(t,r)}r.b=e;var a=new Map;r.a={reflection:Symbol("reflection")}},function(t,r,n){"use strict";function e(t,r){if(!(t instanceof r))throw new TypeError("Cannot call a class as a function")}function a(t,r){for(var n=r||new f,e=t.length-1;e>=0;e--)n=new f(t[e],n);return n}r.b=a;var u=n(2),i=n(0),o=function(){function t(t,r){for(var n=0;n<r.length;n++){var e=r[n];e.enumerable=e.enumerable||!1,e.configurable=!0,"value"in e&&(e.writable=!0),Object.defineProperty(t,e.key,e)}}return function(r,n,e){return n&&t(r.prototype,n),e&&t(r,e),r}}(),f=function(){function t(r,n){e(this,t),this.head=r,this.tail=n}return o(t,[{key:"ToString",value:function(){return"["+Array.from(this).map(function(t){return Object(i.f)(t)}).join("; ")+"]"}},{key:"Equals",value:function(t){if(this===t)return!0;for(var r=this[Symbol.iterator](),n=t[Symbol.iterator]();;){var e=r.next(),a=n.next();if(e.done)return!!a.done;if(a.done)return!1;if(!Object(i.d)(e.value,a.value))return!1}}},{key:"CompareTo",value:function(t){if(this===t)return 0;for(var r=0,n=this[Symbol.iterator](),e=t[Symbol.iterator]();;){var a=n.next(),u=e.next();if(a.done)return u.done?r:-1;if(u.done)return 1;if(0!==(r=Object(i.a)(a.value,u.value)))return r}}},{key:Symbol.iterator,value:function(){var t=this;return{next:function(){var r=t;return t=t.tail,{done:null==r.tail,value:r.head}}}}},{key:u.a.reflection,value:function(){return{type:"Microsoft.FSharp.Collections.FSharpList",interfaces:["System.IEquatable","System.IComparable"]}}},{key:"length",get:function(){for(var t=this,r=0;null!=t.tail;)t=t.tail,r++;return r}}]),t}();r.a=f},function(t,r,n){"use strict";function e(t,r){if(!(t instanceof r))throw new TypeError("Cannot call a class as a function")}function a(t,r){if(null==t){if(!r)throw new Error("Option has no value");return null}return t instanceof o?t.value:t}n.d(r,"a",function(){return o}),r.b=a;var u=n(0),i=function(){function t(t,r){for(var n=0;n<r.length;n++){var e=r[n];e.enumerable=e.enumerable||!1,e.configurable=!0,"value"in e&&(e.writable=!0),Object.defineProperty(t,e.key,e)}}return function(r,n,e){return n&&t(r.prototype,n),e&&t(r,e),r}}(),o=function(){function t(r){e(this,t),this.value=r,this.value=r}return i(t,[{key:"ToString",value:function(){return Object(u.f)(this.value)}},{key:"Equals",value:function(r){return null!=r&&Object(u.d)(this.value,r instanceof t?r.value:r)}},{key:"CompareTo",value:function(r){return null==r?1:Object(u.a)(this.value,r instanceof t?r.value:r)}}]),t}()},function(t,r,n){"use strict";function e(t,r){for(var n=t.toString(10);n.length<r;)n="0"+n;return n}function a(t){var r=t<0;t=Math.abs(t);var n=~~(t/36e5),a=t%36e5/6e4;return(r?"-":"+")+e(n,2)+":"+e(a,2)}function u(t,r){var n=t.toISOString();return"first"===r?n.substring(0,n.indexOf("T")):n.substring(n.indexOf("T")+1,n.length-1)}function i(t,r){if(r)return t.toISOString();var n=null==t.kind||2===t.kind;return e(t.getFullYear(),4)+"-"+e(t.getMonth()+1,2)+"-"+e(t.getDate(),2)+"T"+e(t.getHours(),2)+":"+e(t.getMinutes(),2)+":"+e(t.getSeconds(),2)+"."+e(t.getMilliseconds(),3)+(n?a(-6e4*t.getTimezoneOffset()):"")}function o(t,r){var n=t.toISOString();return n.substring(0,n.length-1)+a(r)}function f(t,r,n){return r.replace(/(\w)\1*/g,function(r){var e=r;switch(r.substring(0,1)){case"y":var a=n?t.getUTCFullYear():t.getFullYear();e=r.length<4?a%100:a;break;case"M":e=(n?t.getUTCMonth():t.getMonth())+1;break;case"d":e=n?t.getUTCDate():t.getDate();break;case"H":e=n?t.getUTCHours():t.getHours();break;case"h":var u=n?t.getUTCHours():t.getHours();e=u>12?u%12:u;break;case"m":e=n?t.getUTCMinutes():t.getMinutes();break;case"s":e=n?t.getUTCSeconds():t.getSeconds()}return e!==r&&e<10&&r.length>1&&(e="0"+e),e})}function c(t,r){var n=new Date(t.getTime()+t.offset);if(!r)return n.toISOString().replace(/\.\d+/,"").replace(/[A-Z]|\.\d+/g," ")+a(t.offset);if(1!==r.length)return f(n,r,!0);switch(r){case"D":case"d":return u(n,"first");case"T":case"t":return u(n,"second");case"O":case"o":return o(n,t.offset);default:throw new Error("Unrecognized Date print format")}}function l(t,r){var n=1===t.kind;if(!r)return n?t.toUTCString():t.toLocaleString();if(1!==r.length)return f(t,r,n);switch(r){case"D":case"d":return n?u(t,"first"):t.toLocaleDateString();case"T":case"t":return n?u(t,"second"):t.toLocaleTimeString();case"O":case"o":return i(t,n);default:throw new Error("Unrecognized Date print format")}}function s(t,r){return null!=t.offset?c(t,r):l(t,r)}function d(t,r){r=null==r?0:r;var n=new Date(t);return n.kind=0|r,n}function v(t,r,n){var e=arguments.length>3&&void 0!==arguments[3]?arguments[3]:0,a=arguments.length>4&&void 0!==arguments[4]?arguments[4]:0,u=arguments.length>5&&void 0!==arguments[5]?arguments[5]:0,i=arguments.length>6&&void 0!==arguments[6]?arguments[6]:0,o=arguments[7],f=1===o?Date.UTC(t,r-1,n,e,a,u,i):new Date(t,r-1,n,e,a,u,i).getTime();if(isNaN(f))throw new Error("The parameters describe an unrepresentable Date.");var c=d(f,o);return t<=99&&c.setFullYear(t,r-1,n),c}function b(t){return 1===t.kind?t.getUTCDate():t.getDate()}function h(t){return(1===t.kind?t.getUTCMonth():t.getMonth())+1}function g(t){return 1===t.kind?t.getUTCFullYear():t.getFullYear()}function y(t,r){var n=t.getTime(),e=r.getTime();return n===e?0:n<e?-1:1}r.b=s,r.a=y},function(t,r,n){"use strict";function e(t,r){return Object(l.b)(function(t,r){return new c.a(r,t)},r,f(t))}function a(t,r){return Object(l.b)(function(r,n){return e(r,t(n))},new c.a,r)}function u(t,r){if(t<0)throw new Error("List length must be non-negative");for(var n=new c.a,e=1;e<=t;e++)n=new c.a(r(t-e),n);return n}function i(t,r){return f(Object(l.b)(function(r,n){return new c.a(t(n),r)},new c.a,r))}function o(t,r){return f(Object(l.b)(function(r,n,e){return new c.a(t(e,n),r)},new c.a,r))}function f(t){return Object(l.b)(function(t,r){return new c.a(r,t)},new c.a,t)}r.a=a,r.c=u,r.d=i,r.e=o,r.g=f;var c=n(3),l=(n(11),n(4),n(1));n.d(r,"f",function(){return c.b}),r.b=c.a},function(t,r,n){"use strict";function e(t,r){for(var n=r.map(function(){return null}),e=new Array(r.length),a=0;a<r.length;a++){var u=t(a);if(u<0||u>=r.length)throw new Error("Not a valid permutation");n[u]=r[a],e[u]=1}for(var i=0;i<r.length;i++)if(1!==e[i])throw new Error("Not a valid permutation");return n}function a(t,r){if(t<1)throw new Error("The input must be positive. parameter name: chunkSize");if(0===r.length)return[[]];for(var n=[],e=0;e<Math.ceil(r.length/t);e++){var a=e*t,u=a+t;n.push(r.slice(a,u))}return n}r.b=e,r.a=a},function(t,r,n){"use strict";function e(t){if(Array.isArray(t)){for(var r=0,n=Array(t.length);r<t.length;r++)n[r]=t[r];return n}return Array.from(t)}function a(t,r){if(!0===t.curried)return t;var n=function(){for(var n=arguments.length,u=Array(n),i=0;i<n;i++)u[i]=arguments[i];var o=Math.max(u.length,1);if(r=Math.max(r||t.length,1),o>=r){var f=u.splice(r),c=t.apply(void 0,u);if("function"==typeof c){var l=a(c);return 0===f.length?l:l.apply(void 0,e(f))}return c}return a(function(){for(var r=arguments.length,n=Array(r),a=0;a<r;a++)n[a]=arguments[a];return t.apply(void 0,e(u.concat(n)))},r-o)};return n.curried=!0,n}function u(t,r){var n=r.map(function(t){return"function"!=typeof t||t.curried?t:a(t)});return(!0===t.curried?t:a(t)).apply(void 0,e(n))}r.a=a,r.b=u},function(t,r,n){"use strict";Object.defineProperty(r,"__esModule",{value:!0});var e=n(10);n.d(r,"Color",function(){return e.a}),n.d(r,"Results",function(){return e.b}),n.d(r,"refX",function(){return e.f}),n.d(r,"refY",function(){return e.g}),n.d(r,"refZ",function(){return e.h}),n.d(r,"round",function(){return e.l}),n.d(r,"toXyz",function(){return e.p}),n.d(r,"toLab",function(){return e.n}),n.d(r,"toRgb",function(){return e.o}),n.d(r,"colors",function(){return e.c}),n.d(r,"renderer",function(){return e.k}),n.d(r,"parseColor",function(){return e.e}),n.d(r,"refresh",function(){return e.i}),n.d(r,"toHex",function(){return e.m}),n.d(r,"createColorDiv",function(){return e.d}),n.d(r,"render",function(){return e.j})},function(t,r,n){"use strict";function e(t,r){if(!(t instanceof r))throw new TypeError("Cannot call a class as a function")}function a(t){return 0|~~Object(b.e)(t)}function u(t){if(1===t.tag)return t;if(2===t.tag){var r=(t.data[0]+16)/116,n=t.data[1]/500+r,e=r-t.data[2]/200,a=function(t){var r=Math.pow(t,3);return r>.008856?r:(t-16/116)/7.787},u=a(n)*S,i=a(r)*j,o=a(e)*C;return new O(1,[u,i,o])}var f=function(t){var r=t/255;return r>.04045?Math.pow((r+.055)/1.055,2.4):r/12.92},c=100*f(t.data[0]),l=100*f(t.data[1]),s=100*f(t.data[2]);return new O(1,[.4124*c+.3576*l+.1805*s,.2126*c+.7152*l+.0722*s,.0193*c+.1192*l+.9505*s])}function i(t){for(;;){if(0!==t.tag){if(1===t.tag){var r=t.data[0]/S,n=t.data[1]/j,e=t.data[2]/C,a=function(t){return t>.008856?Math.pow(t,1/3):7.787*t+16/116},i=a(r),o=a(n),f=a(e);return new O(2,[116*o-16,500*(i-o),200*(o-f)])}return t}t=u(t)}}function o(t){for(;;){if(2!==t.tag){if(1===t.tag){var r=t.data[0]/100,n=t.data[1]/100,e=t.data[2]/100,i=3.2406*r+-1.5372*n+-.4986*e,o=-.9689*r+1.8758*n+.0415*e,f=.0557*r+-.204*n+1.057*e,c=function(t){return t>.0031308?1.055*Math.pow(t,1/2.4)-.055:12.92*t};return new O(0,[a(255*c(i)),a(255*c(o)),a(255*c(f))])}return t}t=u(t)}}function f(t){var r=void 0;r=0===t.indexOf("#")?t.substr(1):t;var n=function(t){return 0|Object(y.a)(t,16)},e=0|n(r.substr(0,2)),a=0|n(r.substr(2,2)),u=0|n(r.substr(4,2));return new O(0,[e,a,u])}function c(){for(var t=document.getElementsByClassName("primary"),r=0|~~t.length,n=new h.b,e=0;e<=r-1;e++){var a=t[e].getElementsByTagName("input")[0],u=a.value;n=new h.b(f(u),n)}return E(Object(h.g)(n)),T()(),null}function l(t){var r=o(t);if(0===r.tag)return Object(p.c)(Object(p.a)("#%X%X%X"))(r.data[0],r.data[1],r.data[2]);throw new Error("shit happended")}function s(t){var r=1===t.tag?[l(t.data),"filled",!0]:[l(t.data),"primary",!1],n=document.createElement("div"),e=document.createElement("input");return e.value=r[0],e.disabled=r[2],e.addEventListener("blur",function(t){return c()}),n.className=r[1],n.style.backgroundColor=r[0],n.appendChild(e),n}function d(){var t=Object(h.c)(3,function(t){return t+3}),r=function(r){return Object(w.h)(Object(w.j)(t,r))}(E()),n=E().length-1|0,e=Object(h.a)(function(t){return t},Object(h.e)(function(t,r){var e=function(t){var r=i(t);if(2===r.tag)return[r.data[0],r.data[1],r.data[2]];throw new Error("this should never happen")},a=e(r[1]),u=e(t===n?E().head:Object(w.d)(t+1,E())),o=function(t,r){return t-r},f=[o(u[0],a[0]),o(u[1],a[1]),o(u[2],a[2])];!function(){var t=Object(p.b)(Object(p.a)("color: %A; oponent: %A; distance: %A"));return Object(g.a)(function(r){var n=Object(g.b)(t,[[r[0],r[1],r[2]]]);return function(t){var r=Object(g.b)(n,[[t[0],t[1],t[2]]]);return function(t){r([t[0],t[1],t[2]])}}})}()([a[0],a[1],a[2]],[u[0],u[1],u[2]],[f[0],f[1],f[2]]);var c=r[0],l=function(t){var r=t+1;return new k(1,new O(2,[a[0]+f[0]/c*r,a[1]+f[1]/c*r,a[2]+f[2]/c*r]))};return new h.b(new k(0,new O(2,[a[0],a[1],a[2]])),Object(h.c)(r[0],l))},r)),a=document.getElementById("wheel");a.innerHTML="",Object(w.e)(function(t){a.appendChild(t)},function(t){return Object(h.d)(function(t){return s(t)},t)}(e))}n.d(r,"a",function(){return O}),n.d(r,"b",function(){return k}),n.d(r,"f",function(){return S}),n.d(r,"g",function(){return j}),n.d(r,"h",function(){return C}),r.l=a,r.p=u,r.n=i,r.o=o,n.d(r,"c",function(){return E}),n.d(r,"k",function(){return T}),r.e=f,r.i=c,r.m=l,r.d=s,r.j=d;var v=n(2),b=n(0),h=n(6),g=n(8),y=n(13),p=n(14),w=n(1),m=function(){function t(t,r){for(var n=0;n<r.length;n++){var e=r[n];e.enumerable=e.enumerable||!1,e.configurable=!0,"value"in e&&(e.writable=!0),Object.defineProperty(t,e.key,e)}}return function(r,n,e){return n&&t(r.prototype,n),e&&t(r,e),r}}(),O=function(){function t(r,n){e(this,t),this.tag=r,this.data=n}return m(t,[{key:v.a.reflection,value:function(){return{type:"colorwheel.Color",interfaces:["FSharpUnion","System.IEquatable","System.IComparable"],cases:[["RGB","number","number","number"],["XYZ","number","number","number"],["LAB","number","number","number"]]}}},{key:"Equals",value:function(t){return this===t||this.tag===t.tag&&Object(b.d)(this.data,t.data)}},{key:"CompareTo",value:function(t){return 0|Object(b.b)(this,t)}}]),t}();Object(v.b)("colorwheel.Color",O);var k=function(){function t(r,n){e(this,t),this.tag=r,this.data=n}return m(t,[{key:v.a.reflection,value:function(){return{type:"colorwheel.Results",interfaces:["FSharpUnion","System.IEquatable","System.IComparable"],cases:[["Primary",O],["Filled",O]]}}},{key:"Equals",value:function(t){return this===t||this.tag===t.tag&&Object(b.d)(this.data,t.data)}},{key:"CompareTo",value:function(t){return 0|Object(b.b)(this,t)}}]),t}();Object(v.b)("colorwheel.Results",k);var S=95.047,j=100,C=108.883,E=Object(b.c)(Object(h.f)([new O(0,[217,64,37]),new O(0,[203,232,143]),new O(0,[183,206,228])])),T=Object(b.c)(Object(g.a)(function(){}));T(function(){d()}),d()},function(t,r,n){"use strict";function e(t,r){if(!(t instanceof r))throw new TypeError("Cannot call a class as a function")}function a(t,r){for(var n=[],e=r[Symbol.iterator](),a=j(),u=e.next();!u.done;){var i=t(u.value),o=E(i,a);null==o?(n.push(i),a=C(i,[u.value],a)):Object(A.b)(o).push(u.value),u=e.next()}return n.map(function(t){return[t,a.get(t)]})}function u(t,r){for(;;){if(1===r.tag)return t+1|0;{if(2!==r.tag)return 0|t;t=u(t+1,r.data[2]),r=r.data[3]}}}function i(t){return u(0,t)}function o(){return new N(0)}function f(t){return 1===t.tag?1:2===t.tag?t.data[4]:0}function c(t,r,n,e){switch(0===t.tag&&0===e.tag?0:1){case 0:return new N(1,[r,n]);case 1:var a=0|f(t),u=0|f(e);return new N(2,[r,n,t,e,1+(0|(a<u?u:a))])}throw new Error("internal error: Map.tree_mk")}function l(t,r,n,e){var a=f(t),u=f(e);if(u>a+2){if(2===e.tag){if(f(e.data[2])>a+1){if(2===e.data[2].tag)return c(c(t,r,n,e.data[2].data[2]),e.data[2].data[0],e.data[2].data[1],c(e.data[2].data[3],e.data[0],e.data[1],e.data[3]));throw new Error("rebalance")}return c(c(t,r,n,e.data[2]),e.data[0],e.data[1],e.data[3])}throw new Error("rebalance")}if(a>u+2){if(2===t.tag){if(f(t.data[3])>u+1){if(2===t.data[3].tag)return c(c(t.data[2],t.data[0],t.data[1],t.data[3].data[2]),t.data[3].data[0],t.data[3].data[1],c(t.data[3].data[3],r,n,e));throw new Error("rebalance")}return c(t.data[2],t.data[0],t.data[1],c(t.data[3],r,n,e))}throw new Error("rebalance")}return c(t,r,n,e)}function s(t,r,n,e){if(1===e.tag){var a=t.Compare(r,e.data[0]);return a<0?new N(2,[r,n,new N(0),e,2]):0===a?new N(1,[r,n]):new N(2,[r,n,e,new N(0),2])}if(2===e.tag){var u=t.Compare(r,e.data[0]);return u<0?l(s(t,r,n,e.data[2]),e.data[0],e.data[1],e.data[3]):0===u?new N(2,[r,n,e.data[2],e.data[3],e.data[4]]):l(e.data[2],e.data[0],e.data[1],s(t,r,n,e.data[3]))}return new N(1,[r,n])}function d(t,r,n){var e=v(t,r,n);if(null==e)throw new Error("key not found: "+r);return Object(A.b)(e)}function v(t,r,n){t:for(;;){if(1===n.tag){var e=0|t.Compare(r,n.data[0]);return 0===e?new A.a(n.data[1]):null}{if(2!==n.tag)return null;var a=0|t.Compare(r,n.data[0]);if(a<0){t=t,r=r,n=n.data[2];continue t}if(0===a)return new A.a(n.data[1]);t=t,r=r,n=n.data[3]}}}function b(t){if(1===t.tag)return[t.data[0],t.data[1],new N(0)];if(2===t.tag){if(0===t.data[2].tag)return[t.data[0],t.data[1],t.data[3]];var r=b(t.data[2]);return[r[0],r[1],c(r[2],t.data[0],t.data[1],t.data[3])]}throw new Error("internal error: Map.spliceOutSuccessor")}function h(t,r,n){if(1===n.tag){return 0===t.Compare(r,n.data[0])?new N(0):n}if(2===n.tag){var e=t.Compare(r,n.data[0]);if(e<0)return l(h(t,r,n.data[2]),n.data[0],n.data[1],n.data[3]);if(0===e){if(0===n.data[2].tag)return n.data[3];if(0===n.data[3].tag)return n.data[2];var a=b(n.data[3]);return c(n.data[2],a[0],a[1],a[2])}return l(n.data[2],n.data[0],n.data[1],h(t,r,n.data[3]))}return o()}function g(t,r,n){t:for(;;){if(1===n.tag)return 0===t.Compare(r,n.data[0]);{if(2!==n.tag)return!1;var e=0|t.Compare(r,n.data[0]);if(e<0){t=t,r=r,n=n.data[2];continue t}if(0===e)return!0;t=t,r=r,n=n.data[3]}}}function y(t,r){return 1===r.tag?new N(1,[r.data[0],t(r.data[0],r.data[1])]):2===r.tag?new N(2,[r.data[0],t(r.data[0],r.data[1]),y(t,r.data[2]),y(t,r.data[3]),r.data[4]]):o()}function p(t,r,n){for(var e=n.next();!e.done;)r=s(t,e.value[0],e.value[1],r),e=n.next();return r}function w(t,r){var n=r[Symbol.iterator]();return p(t,o(),n)}function m(t){return null!=t.tail?1===t.head.tag?t:m(2===t.head.tag?Object(x.b)([t.head.data[2],new N(1,[t.head.data[0],t.head.data[1]]),t.head.data[3]],t.tail):t.tail):new x.a}function O(t){return{stack:m(new x.a(t,new x.a)),started:!1}}function k(t){function r(t){if(null==t.stack.tail)return null;if(1===t.stack.head.tag)return[t.stack.head.data[0],t.stack.head.data[1]];throw new Error("Please report error: Map iterator, unexpected stack for current")}if(t.started){if(null==t.stack.tail)return{done:!0,value:null};if(1===t.stack.head.tag)return t.stack=m(t.stack.tail),{done:null==t.stack.tail,value:r(t)};throw new Error("Please report error: Map iterator, unexpected stack for moveNext")}return t.started=!0,{done:null==t.stack.tail,value:r(t)}}function S(t,r){var n=new P;return n.tree=r,n.comparer=t||new T.a,n}function j(t,r){return r=r||new T.a,S(r,t?w(r,t):o())}function C(t,r,n){return S(n.comparer,s(n.comparer,t,r,n.tree))}function E(t,r){return v(r.comparer,t,r.tree)}r.a=a;var T=n(12),x=n(3),A=n(4),M=n(1),D=n(2),U=n(0),I=function(){function t(t,r){for(var n=0;n<r.length;n++){var e=r[n];e.enumerable=e.enumerable||!1,e.configurable=!0,"value"in e&&(e.writable=!0),Object.defineProperty(t,e.key,e)}}return function(r,n,e){return n&&t(r.prototype,n),e&&t(r,e),r}}(),N=function t(r,n){e(this,t),this.tag=0|r,this.data=n},P=function(){function t(){e(this,t)}return I(t,[{key:"ToString",value:function(){return"map ["+Array.from(this).map(function(t){return Object(U.f)(t)}).join("; ")+"]"}},{key:"Equals",value:function(t){return 0===this.CompareTo(t)}},{key:"CompareTo",value:function(t){var r=this;return this===t?0:Object(M.a)(function(t,n){var e=r.comparer.Compare(t[0],n[0]);return 0!==e?e:Object(U.a)(t[1],n[1])},this,t)}},{key:Symbol.iterator,value:function(){var t=O(this.tree);return{next:function(){return k(t)}}}},{key:"entries",value:function(){return this[Symbol.iterator]()}},{key:"keys",value:function(){return Object(M.f)(function(t){return t[0]},this)}},{key:"values",value:function(){return Object(M.f)(function(t){return t[1]},this)}},{key:"get",value:function(t){return d(this.comparer,t,this.tree)}},{key:"has",value:function(t){return g(this.comparer,t,this.tree)}},{key:"set",value:function(t,r){this.tree=s(this.comparer,t,r,this.tree)}},{key:"delete",value:function(t){var r=i(this.tree);return this.tree=h(this.comparer,t,this.tree),r>i(this.tree)}},{key:"clear",value:function(){this.tree=o()}},{key:D.a.reflection,value:function(){return{type:"Microsoft.FSharp.Collections.FSharpMap",interfaces:["System.IEquatable","System.IComparable","System.Collections.Generic.IDictionary"]}}},{key:"size",get:function(){return i(this.tree)}}]),t}()},function(t,r,n){"use strict";function e(t,r){if(!(t instanceof r))throw new TypeError("Cannot call a class as a function")}var a=n(2),u=n(0),i=function(){function t(t,r){for(var n=0;n<r.length;n++){var e=r[n];e.enumerable=e.enumerable||!1,e.configurable=!0,"value"in e&&(e.writable=!0),Object.defineProperty(t,e.key,e)}}return function(r,n,e){return n&&t(r.prototype,n),e&&t(r,e),r}}(),o=function(){function t(r){e(this,t),this.Compare=r||u.a}return i(t,[{key:a.a.reflection,value:function(){return{interfaces:["System.IComparer"]}}}]),t}();r.a=o},function(t,r,n){"use strict";function e(t,r){var n=o.exec(t);if(null!=n){if(null==r)switch(n[2]){case"0b":r=2;break;case"0o":r=8;break;case"0x":r=16;break;default:r=10}switch(r){case 2:return f.test(n[3])?null:[n,2];case 8:return c.test(n[3])?null:[n,8];case 10:return l.test(n[3])?null:[n,10];case 16:return[n,16];default:throw new Error("Invalid Base.")}}return null}function a(t,r,n){var a=e(t,r);if(null!==a){var u=i(a,2),o=i(u[0],4),f=o[1],c=o[3],l=u[1],s=parseInt((f||"")+c,l);if(!Number.isNaN(s))return[!0,s]}return[!1,n]}function u(t,r){var n=a(t,r,0);if(n[0])return n[1];throw new Error("Input string was not in a correct format.")}r.a=u;var i=function(){function t(t,r){var n=[],e=!0,a=!1,u=void 0;try{for(var i,o=t[Symbol.iterator]();!(e=(i=o.next()).done)&&(n.push(i.value),!r||n.length!==r);e=!0);}catch(t){a=!0,u=t}finally{try{!e&&o.return&&o.return()}finally{if(a)throw u}}return n}return function(r,n){if(Array.isArray(r))return r;if(Symbol.iterator in Object(r))return t(r,n);throw new TypeError("Invalid attempt to destructure non-iterable instance")}}(),o=/^\s*([\+\-])?(0[xob])?([0-9a-fA-F]+)\s*$/,f=/[^01]/,c=/[^0-7]/,l=/[^0-9]/},function(t,r,n){"use strict";function e(t){return t<0?"ff"+(16777215-(Math.abs(t)-1)).toString(16):t.toString(16)}function a(t){return{input:t,cont:c(t)}}function u(t){return t.cont(function(t){console.log(t)})}function i(t){return t.cont(function(t){return t})}function o(t,r){return t.replace(v,function(t,n,a,u,i,o){switch(o){case"f":case"F":r=r.toFixed(i||6);break;case"g":case"G":r=r.toPrecision(i);break;case"e":case"E":r=r.toExponential(i);break;case"O":r=Object(d.f)(r);break;case"A":r=Object(d.f)(r,!0);break;case"x":r=e(Number(r));break;case"X":r=e(Number(r)).toUpperCase()}var f=a.indexOf("+")>=0&&parseInt(r,10)>=0;if(u=parseInt(u,10),!isNaN(u)){var c=u>=0&&a.indexOf("0")>=0?"0":" ";r=l(r,Math.abs(u)-(f?1:0),c,u<0)}return(n+(f?"+"+r:r)).replace(/%/g,"%%")})}function f(t,r){var n=function(){for(var n=arguments.length,e=Array(n),a=0;a<n;a++)e[a]=arguments[a];var u=t,i=!0,c=!1,l=void 0;try{for(var s,d=e[Symbol.iterator]();!(i=(s=d.next()).done);i=!0){u=o(u,s.value)}}catch(t){c=!0,l=t}finally{try{!i&&d.return&&d.return()}finally{if(c)throw l}}return v.test(u)?f(u,r):r(u.replace(/%%/g,"%"))};return n.curried=!0,n}function c(t){return function(r){return v.test(t)?f(t,r):r(t)}}function l(t,r,n,e){n=n||" ",t=String(t),r-=t.length;for(var a=0;a<r;a++)t=e?t+n:n+t;return t}r.a=a,r.b=u,r.c=i;var s=n(5),d=(n(15),n(0)),v=/(^|[^%])%([0+ ]*)(-?\d+)?(?:\.(\d+))?(\w)/,b=/\{(\d+)(,-?\d+)?(?:\:(.+?))?\}/g},function(t,r,n){"use strict";function e(t){return t.replace(/[\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|]/g,"\\$&")}r.a=e}]);
//# sourceMappingURL=bundle.js.map