!function(t){function n(r){if(e[r])return e[r].exports;var a=e[r]={i:r,l:!1,exports:{}};return t[r].call(a.exports,a,a.exports,n),a.l=!0,a.exports}var e={};n.m=t,n.c=e,n.d=function(t,e,r){n.o(t,e)||Object.defineProperty(t,e,{configurable:!1,enumerable:!0,get:r})},n.n=function(t){var e=t&&t.__esModule?function(){return t.default}:function(){return t};return n.d(e,"a",e),e},n.o=function(t,n){return Object.prototype.hasOwnProperty.call(t,n)},n.p="",n(n.s=8)}([function(t,n,e){"use strict";function r(t,n){if(!(t instanceof n))throw new TypeError("Cannot call a class as a function")}function a(t,n){if("System.Collections.Generic.IEnumerable"===n)return"function"==typeof t[Symbol.iterator];if("function"==typeof t[h.a.reflection]){var e=t[h.a.reflection]().interfaces;return Array.isArray(e)&&e.indexOf(n)>-1}return!1}function u(t){if(null==t)return[];var n="function"==typeof t[h.a.reflection]?t[h.a.reflection]().properties||[]:t;return Object.getOwnPropertyNames(n)}function o(t){function n(t){return!(null===t||"object"!==(void 0===t?"undefined":g(t))||t instanceof Number||t instanceof String||t instanceof Boolean)}var e=arguments.length>1&&void 0!==arguments[1]&&arguments[1];if(null==t||"number"==typeof t)return String(t);if("string"==typeof t)return e?JSON.stringify(t):t;if(t instanceof Date)return Object(b.b)(t);if("function"==typeof t.ToString)return t.ToString();if(a(t,"FSharpUnion")){var r=t[h.a.reflection](),u=r.cases[t.tag];switch(u.length){case 1:return u[0];case 2:return u[0]+" ("+o(t.data,!0)+")";default:return u[0]+" ("+t.data.map(function(t){return o(t,!0)}).join(",")+")"}}try{return JSON.stringify(t,function(t,e){return e&&e[Symbol.iterator]&&!Array.isArray(e)&&n(e)?Array.from(e):e&&"function"==typeof e.ToString?o(e):e})}catch(n){return"{"+Object.getOwnPropertyNames(t).map(function(n){return n+": "+String(t[n])}).join(", ")+"}"}}function i(t){if(null!=t&&"function"==typeof t.GetHashCode)return t.GetHashCode();for(var n=o(t),e=5381,r=0,a=n.length;r<a;)e=33*e^n.charCodeAt(r++);return e}function c(t,n){if(t===n)return!0;if(null==t)return null==n;if(null==n)return!1;if("function"==typeof t.Equals)return t.Equals(n);if("function"==typeof n.Equals)return n.Equals(t);if(Object.getPrototypeOf(t)!==Object.getPrototypeOf(n))return!1;if(Array.isArray(t)){if(t.length!==n.length)return!1;for(var e=0;e<t.length;e++)if(!c(t[e],n[e]))return!1;return!0}if(ArrayBuffer.isView(t)){if(t.byteLength!==n.byteLength)return!1;for(var r=new DataView(t.buffer),a=new DataView(n.buffer),u=0;u<t.byteLength;u++)if(r.getUint8(u)!==a.getUint8(u))return!1;return!0}return t instanceof Date&&t.getTime()===n.getTime()}function f(t,n){if(t===n)return 0;if(null==t)return null==n?0:-1;if(null==n)return 1;if("function"==typeof t.CompareTo)return t.CompareTo(n);if("function"==typeof n.CompareTo)return-1*n.CompareTo(t);if(Object.getPrototypeOf(t)!==Object.getPrototypeOf(n))return-1;if(Array.isArray(t)){if(t.length!==n.length)return t.length<n.length?-1:1;for(var e=0,r=0;e<t.length;e++)if(0!==(r=f(t[e],n[e])))return r;return 0}if(ArrayBuffer.isView(t)){if(t.byteLength!==n.byteLength)return t.byteLength<n.byteLength?-1:1;for(var a=new DataView(t.buffer),u=new DataView(n.buffer),o=0,l=0,s=0;o<t.byteLength;o++){if(l=a.getUint8(o),s=u.getUint8(o),l<s)return-1;if(l>s)return 1}return 0}if(t instanceof Date)return Object(b.a)(t,n);if("object"===(void 0===t?"undefined":g(t))){var d=i(t),v=i(n);return d===v?c(t,n)?0:-1:d<v?-1:1}return t<n?-1:1}function l(t,n){if(t===n)return!0;var e=u(t),r=!0,a=!1,o=void 0;try{for(var i,f=e[Symbol.iterator]();!(r=(i=f.next()).done);r=!0){var l=i.value;if(!c(t[l],n[l]))return!1}}catch(t){a=!0,o=t}finally{try{!r&&f.return&&f.return()}finally{if(a)throw o}}return!0}function s(t,n){if(t===n)return 0;var e=t.tag<n.tag?-1:t.tag>n.tag?1:0;return 0!==e?e:f(t.data,n.data)}function d(t){var n=t;return function(){return 0===arguments.length?n:void(n=arguments[0])}}function v(t){var n=arguments.length>1&&void 0!==arguments[1]?arguments[1]:0,e=Math.pow(10,n),r=+(n?t*e:t).toFixed(8),a=Math.floor(r),u=r-a,o=u>.5-1e-8&&u<.5+1e-8?a%2==0?a:a+1:Math.round(r);return n?o/e:o}n.f=o,n.d=c,n.a=f,n.b=s,n.c=d,n.e=v;var b=e(5),h=e(2),g="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t},y=function(){function t(t,n){for(var e=0;e<n.length;e++){var r=n[e];r.enumerable=r.enumerable||!1,r.configurable=!0,"value"in r&&(r.writable=!0),Object.defineProperty(t,r.key,r)}}return function(n,e,r){return e&&t(n.prototype,e),r&&t(n,r),n}}(),p=function(){function t(n,e,a){r(this,t),this.kind=n,this.definition=e,this.generics=a}return y(t,[{key:"Equals",value:function(t){return this.kind===t.kind&&this.definition===t.definition&&("object"===g(this.generics)?l(this.generics,t.generics):this.generics===t.generics)}}]),t}();new p("Any"),new p("Unit")},function(t,n,e){"use strict";function r(t,n,e){return n in t?Object.defineProperty(t,n,{value:e,enumerable:!0,configurable:!0,writable:!0}):t[n]=e,t}function a(t,n){if(!(t instanceof n))throw new TypeError("Cannot call a class as a function")}function u(t){if(null==t)throw new Error("Seq did not contain any matching element");return Object(I.c)(t)}function o(t){return l(function(t,n){return new A.a(t,n)},t,new A.a)}function i(t,n,e){var r=k(function(t){return 0!==t},p(function(n,e){return t(n,e)},n,e));return null!=r?Object(I.c)(r):g(n)-g(e)}function c(t){return r({},Symbol.iterator,function(){return t()[Symbol.iterator]()})}function f(t,n,e){if(Array.isArray(e)||ArrayBuffer.isView(e))return e.reduce(t,n);for(var r=void 0,a=0,u=e[Symbol.iterator]();r=u.next(),!r.done;a++)n=t(n,r.value,a);return n}function l(t,n,e){for(var r=Array.isArray(n)||ArrayBuffer.isView(n)?n:Array.from(n),a=r.length-1;a>=0;a--)e=t(r[a],e,a);return e}function s(t,n){if(t<0)return null;if(Array.isArray(n)||ArrayBuffer.isView(n))return t<n.length?new I.a(n[t]):null;for(var e=0,r=n[Symbol.iterator]();;e++){var a=r.next();if(a.done)break;if(e===t)return new I.a(a.value)}return null}function d(t,n){return u(s(t,n))}function v(t,n){f(function(n,e){return t(e)},null,n)}function b(t,n){f(function(n,e,r){return t(r,e)},null,n)}function h(t){try{return new I.a(O(function(t,n){return n},t))}catch(t){return null}}function g(t){return Array.isArray(t)||ArrayBuffer.isView(t)?t.length:f(function(t,n){return t+1},0,t)}function y(t,n){return c(function(){return x(function(n){var e=n.next();return e.done?null:[t(e.value),n]},n[Symbol.iterator]())})}function p(t,n,e){return c(function(){var r=n[Symbol.iterator](),a=e[Symbol.iterator]();return x(function(){var n=r.next(),e=a.next();return n.done||e.done?null:[t(n.value,e.value),null]})})}function m(t,n,e){if(0===n)throw new Error("Step cannot be 0");return c(function(){return x(function(t){return n>0&&t<=e||n<0&&t>=e?[t,t+n]:null},t)})}function w(t,n){return m(t,1,n)}function O(t,n){if(Array.isArray(n)||ArrayBuffer.isView(n))return n.reduce(t);var e=n[Symbol.iterator](),r=e.next();if(r.done)throw new Error("Seq was empty");for(var a=r.value;;){if(r=e.next(),r.done)break;a=t(a,r.value)}return a}function j(t,n){return r({},Symbol.iterator,function(){for(var e=n[Symbol.iterator](),r=1;r<=t;r++)if(e.next().done)throw new Error("Seq has not enough elements");return e})}function S(t,n){var e=arguments.length>2&&void 0!==arguments[2]&&arguments[2];return c(function(){var r=n[Symbol.iterator]();return x(function(n){if(n<t){var a=r.next();if(!a.done)return[a.value,n+1];if(!e)throw new Error("Seq has not enough elements")}},0)})}function k(t,n,e){for(var r=0,a=n[Symbol.iterator]();;r++){var u=a.next();if(u.done)break;if(t(u.value,r))return new I.a(u.value)}return void 0===e?null:new I.a(e)}function E(t,n){for(var e=0,r=n[Symbol.iterator]();;e++){var a=r.next();if(a.done)break;var u=t(a.value,e);if(null!=u)return u}return null}function C(t,n){return u(E(t,n))}function x(t,n){return r({},Symbol.iterator,function(){var e=n;return{next:function(){var n=t(e);return null!=n?(e=n[1],{done:!1,value:n[0]}):{done:!0}}}})}function T(t,n){return p(function(t,n){return[t,n]},t,n)}n.l=o,n.a=i,n.b=f,n.c=l,n.d=d,n.e=v,n.f=b,n.g=y,n.i=w,n.j=j,n.k=S,n.m=E,n.h=C,n.n=T;var A=(e(7),e(3)),I=e(4),M=(e(0),function(){function t(t,n){var e=[],r=!0,a=!1,u=void 0;try{for(var o,i=t[Symbol.iterator]();!(r=(o=i.next()).done)&&(e.push(o.value),!n||e.length!==n);r=!0);}catch(t){a=!0,u=t}finally{try{!r&&i.return&&i.return()}finally{if(a)throw u}}return e}}(),function(){function t(t,n){for(var e=0;e<n.length;e++){var r=n[e];r.enumerable=r.enumerable||!1,r.configurable=!0,"value"in r&&(r.writable=!0),Object.defineProperty(t,r.key,r)}}return function(n,e,r){return e&&t(n.prototype,e),r&&t(n,r),n}}());!function(){function t(n){a(this,t),this.iter=n}M(t,[{key:"MoveNext",value:function(){var t=this.iter.next();return this.current=t.value,!t.done}},{key:"Reset",value:function(){throw new Error("JS iterators cannot be reset")}},{key:"Dispose",value:function(){}},{key:"Current",get:function(){return this.current}},{key:"get_Current",get:function(){return this.current}}])}()},function(t,n,e){"use strict";function r(t,n){a.set(t,n)}n.b=r;var a=new Map;n.a={reflection:Symbol("reflection")}},function(t,n,e){"use strict";function r(t,n){if(!(t instanceof n))throw new TypeError("Cannot call a class as a function")}function a(t,n){for(var e=n||new c,r=t.length-1;r>=0;r--)e=new c(t[r],e);return e}n.b=a;var u=e(2),o=e(0),i=function(){function t(t,n){for(var e=0;e<n.length;e++){var r=n[e];r.enumerable=r.enumerable||!1,r.configurable=!0,"value"in r&&(r.writable=!0),Object.defineProperty(t,r.key,r)}}return function(n,e,r){return e&&t(n.prototype,e),r&&t(n,r),n}}(),c=function(){function t(n,e){r(this,t),this.head=n,this.tail=e}return i(t,[{key:"ToString",value:function(){return"["+Array.from(this).map(function(t){return Object(o.f)(t)}).join("; ")+"]"}},{key:"Equals",value:function(t){if(this===t)return!0;for(var n=this[Symbol.iterator](),e=t[Symbol.iterator]();;){var r=n.next(),a=e.next();if(r.done)return!!a.done;if(a.done)return!1;if(!Object(o.d)(r.value,a.value))return!1}}},{key:"CompareTo",value:function(t){if(this===t)return 0;for(var n=0,e=this[Symbol.iterator](),r=t[Symbol.iterator]();;){var a=e.next(),u=r.next();if(a.done)return u.done?n:-1;if(u.done)return 1;if(0!==(n=Object(o.a)(a.value,u.value)))return n}}},{key:Symbol.iterator,value:function(){var t=this;return{next:function(){var n=t;return t=t.tail,{done:null==n.tail,value:n.head}}}}},{key:u.a.reflection,value:function(){return{type:"Microsoft.FSharp.Collections.FSharpList",interfaces:["System.IEquatable","System.IComparable"]}}},{key:"length",get:function(){for(var t=this,n=0;null!=t.tail;)t=t.tail,n++;return n}}]),t}();n.a=c},function(t,n,e){"use strict";function r(t,n){if(!(t instanceof n))throw new TypeError("Cannot call a class as a function")}function a(t,n){if(null==t){if(!n)throw new Error("Option has no value");return null}return t instanceof c?t.value:t}function u(t,n,e){return null==t?n:null!=e?e(a(t)):a(t)}e.d(n,"a",function(){return c}),n.c=a,n.b=u;var o=e(0),i=function(){function t(t,n){for(var e=0;e<n.length;e++){var r=n[e];r.enumerable=r.enumerable||!1,r.configurable=!0,"value"in r&&(r.writable=!0),Object.defineProperty(t,r.key,r)}}return function(n,e,r){return e&&t(n.prototype,e),r&&t(n,r),n}}(),c=function(){function t(n){r(this,t),this.value=n,this.value=n}return i(t,[{key:"ToString",value:function(){return Object(o.f)(this.value)}},{key:"Equals",value:function(n){return null!=n&&Object(o.d)(this.value,n instanceof t?n.value:n)}},{key:"CompareTo",value:function(n){return null==n?1:Object(o.a)(this.value,n instanceof t?n.value:n)}}]),t}()},function(t,n,e){"use strict";function r(t,n){for(var e=t.toString(10);e.length<n;)e="0"+e;return e}function a(t){var n=t<0;t=Math.abs(t);var e=~~(t/36e5),a=t%36e5/6e4;return(n?"-":"+")+r(e,2)+":"+r(a,2)}function u(t,n){var e=t.toISOString();return"first"===n?e.substring(0,e.indexOf("T")):e.substring(e.indexOf("T")+1,e.length-1)}function o(t,n){if(n)return t.toISOString();var e=null==t.kind||2===t.kind;return r(t.getFullYear(),4)+"-"+r(t.getMonth()+1,2)+"-"+r(t.getDate(),2)+"T"+r(t.getHours(),2)+":"+r(t.getMinutes(),2)+":"+r(t.getSeconds(),2)+"."+r(t.getMilliseconds(),3)+(e?a(-6e4*t.getTimezoneOffset()):"")}function i(t,n){var e=t.toISOString();return e.substring(0,e.length-1)+a(n)}function c(t,n,e){return n.replace(/(\w)\1*/g,function(n){var r=n;switch(n.substring(0,1)){case"y":var a=e?t.getUTCFullYear():t.getFullYear();r=n.length<4?a%100:a;break;case"M":r=(e?t.getUTCMonth():t.getMonth())+1;break;case"d":r=e?t.getUTCDate():t.getDate();break;case"H":r=e?t.getUTCHours():t.getHours();break;case"h":var u=e?t.getUTCHours():t.getHours();r=u>12?u%12:u;break;case"m":r=e?t.getUTCMinutes():t.getMinutes();break;case"s":r=e?t.getUTCSeconds():t.getSeconds()}return r!==n&&r<10&&n.length>1&&(r="0"+r),r})}function f(t,n){var e=new Date(t.getTime()+t.offset);if(!n)return e.toISOString().replace(/\.\d+/,"").replace(/[A-Z]|\.\d+/g," ")+a(t.offset);if(1!==n.length)return c(e,n,!0);switch(n){case"D":case"d":return u(e,"first");case"T":case"t":return u(e,"second");case"O":case"o":return i(e,t.offset);default:throw new Error("Unrecognized Date print format")}}function l(t,n){var e=1===t.kind;if(!n)return e?t.toUTCString():t.toLocaleString();if(1!==n.length)return c(t,n,e);switch(n){case"D":case"d":return e?u(t,"first"):t.toLocaleDateString();case"T":case"t":return e?u(t,"second"):t.toLocaleTimeString();case"O":case"o":return o(t,e);default:throw new Error("Unrecognized Date print format")}}function s(t,n){return null!=t.offset?f(t,n):l(t,n)}function d(t,n){n=null==n?0:n;var e=new Date(t);return e.kind=0|n,e}function v(t,n,e){var r=arguments.length>3&&void 0!==arguments[3]?arguments[3]:0,a=arguments.length>4&&void 0!==arguments[4]?arguments[4]:0,u=arguments.length>5&&void 0!==arguments[5]?arguments[5]:0,o=arguments.length>6&&void 0!==arguments[6]?arguments[6]:0,i=arguments[7],c=1===i?Date.UTC(t,n-1,e,r,a,u,o):new Date(t,n-1,e,r,a,u,o).getTime();if(isNaN(c))throw new Error("The parameters describe an unrepresentable Date.");var f=d(c,i);return t<=99&&f.setFullYear(t,n-1,e),f}function b(t){return 1===t.kind?t.getUTCDate():t.getDate()}function h(t){return(1===t.kind?t.getUTCMonth():t.getMonth())+1}function g(t){return 1===t.kind?t.getUTCFullYear():t.getFullYear()}function y(t,n){var e=t.getTime(),r=n.getTime();return e===r?0:e<r?-1:1}n.b=s,n.a=y},function(t,n,e){"use strict";function r(t,n){return Object(l.b)(function(t,n){return new f.a(n,t)},n,c(t))}function a(t,n){return Object(l.b)(function(n,e){return r(n,t(e))},new f.a,n)}function u(t,n){if(t<0)throw new Error("List length must be non-negative");for(var e=new f.a,r=1;r<=t;r++)e=new f.a(n(t-r),e);return e}function o(t,n){return c(Object(l.b)(function(n,e){return new f.a(t(e),n)},new f.a,n))}function i(t,n){return c(Object(l.b)(function(n,e,r){return new f.a(t(r,e),n)},new f.a,n))}function c(t){return Object(l.b)(function(t,n){return new f.a(n,t)},new f.a,t)}n.a=a,n.c=u,n.d=o,n.e=i,n.g=c;var f=e(3),l=(e(10),e(4),e(1));e.d(n,"f",function(){return f.b}),n.b=f.a},function(t,n,e){"use strict";function r(t,n){for(var e=n.map(function(){return null}),r=new Array(n.length),a=0;a<n.length;a++){var u=t(a);if(u<0||u>=n.length)throw new Error("Not a valid permutation");e[u]=n[a],r[u]=1}for(var o=0;o<n.length;o++)if(1!==r[o])throw new Error("Not a valid permutation");return e}function a(t,n){if(t<1)throw new Error("The input must be positive. parameter name: chunkSize");if(0===n.length)return[[]];for(var e=[],r=0;r<Math.ceil(n.length/t);r++){var a=r*t,u=a+t;e.push(n.slice(a,u))}return e}n.b=r,n.a=a},function(t,n,e){"use strict";Object.defineProperty(n,"__esModule",{value:!0});var r=e(9);e.d(n,"Color",function(){return r.a}),e.d(n,"Results",function(){return r.c}),e.d(n,"RefreshFrom",function(){return r.b}),e.d(n,"refX",function(){return r.j}),e.d(n,"refY",function(){return r.k}),e.d(n,"refZ",function(){return r.l}),e.d(n,"round",function(){return r.q}),e.d(n,"toXyz",function(){return r.v}),e.d(n,"toLab",function(){return r.t}),e.d(n,"toRgb",function(){return r.u}),e.d(n,"save",function(){return r.r}),e.d(n,"load",function(){return r.h}),e.d(n,"colors",function(){return r.e}),e.d(n,"inputs",function(){return r.g}),e.d(n,"rgbInputs",function(){return r.p}),e.d(n,"renderer",function(){return r.o}),e.d(n,"parseColor",function(){return r.i}),e.d(n,"toHex",function(){return r.s}),e.d(n,"createColorDiv",function(){return r.f}),e.d(n,"updateColorBoxes",function(){return r.w}),e.d(n,"updateRgbBoxes",function(){return r.x}),e.d(n,"refresh",function(){return r.m}),e.d(n,"render",function(){return r.n}),e.d(n,"colorDivs",function(){return r.d})},function(t,n,e){"use strict";function r(t,n){if(!(t instanceof n))throw new TypeError("Cannot call a class as a function")}function a(t){return 0|~~Object(y.e)(t)}function u(t){if(1===t.tag)return t;if(2===t.tag){var n=(t.data[0]+16)/116,e=t.data[1]/500+n,r=n-t.data[2]/200,a=function(t){var n=Math.pow(t,3);return n>.008856?n:(t-16/116)/7.787},u=a(e)*T,o=a(n)*A,i=a(r)*I;return new E(1,[u,o,i])}var c=function(t){var n=t/255;return n>.04045?Math.pow((n+.055)/1.055,2.4):n/12.92},f=100*c(t.data[0]),l=100*c(t.data[1]),s=100*c(t.data[2]);return new E(1,[.4124*f+.3576*l+.1805*s,.2126*f+.7152*l+.0722*s,.0193*f+.1192*l+.9505*s])}function o(t){for(;;){if(0!==t.tag){if(1===t.tag){var n=t.data[0]/T,e=t.data[1]/A,r=t.data[2]/I,a=function(t){return t>.008856?Math.pow(t,1/3):7.787*t+16/116},o=a(n),i=a(e),c=a(r);return new E(2,[116*i-16,500*(o-i),200*(i-c)])}return t}t=u(t)}}function i(t){for(;;){if(2!==t.tag){if(1===t.tag){var n=t.data[0]/100,e=t.data[1]/100,r=t.data[2]/100,o=3.2406*n+-1.5372*e+-.4986*r,i=-.9689*n+1.8758*e+.0415*r,c=.0557*n+-.204*e+1.057*r,f=function(t){return t>.0031308?1.055*Math.pow(t,1/2.4)-.055:12.92*t};return new E(0,[a(255*f(o)),a(255*f(i)),a(255*f(c))])}return t}t=u(t)}}function c(t){var n=Array.from(t);localStorage.setItem("colors",JSON.stringify(n))}function f(t){var n=void 0;n=0===t.indexOf("#")?t.substr(1):t;var e=function(t){return 0|Object(O.a)(t,16)},r=0|e(n.substr(0,2)),a=0|e(n.substr(2,2)),u=0|e(n.substr(4,2));return new E(0,[r,a,u])}function l(t){var n=i(t);if(0===n.tag)return Object(j.c)(Object(j.a)("#%X%X%X"))(n.data[0],n.data[1],n.data[2]);throw new Error("shit happended")}function s(t){var n=1===t.tag?[l(t.data),"filled"]:[l(t.data),"primary"],e=document.createElement("div");return e.className=n[1],e.style.backgroundColor=n[0],e}function d(){for(var t=0;t<=~~D.length-1;t++)!function(t){D[t].value=l(function(n){return Object(S.d)(t,n)}(B()))}(t)}function v(){Object(S.f)(function(t,n){var e=i(Object(S.d)(~~(t/3),B())),r=void 0;if(0!==e.tag)throw new Error("should never happen");r=[e.data[0],e.data[1],e.data[2]];var a=t%3|0;if(0===a)n.value=r[0].toString();else if(1===a)n.value=r[1].toString();else{if(2!==a)throw new Error("/Users/kdummann/source/colorwheel/src/App.fs",280,14);n.value=r[2].toString()}},U)}function b(t){var n=0|~~D.length,e=new m.b;if(1===t.tag)e=Object(m.d)(function(t){Object(j.b)(Object(j.a)("%d"))(t);var n=3*t|0,e=Object(m.d)(function(t){return Object(O.a)(t.value,10)},Object(S.l)(Object(S.k)(3,function(t){return Object(S.l)(Object(S.j)(n,t))}(U)))),r=0|Object(S.d)(0,e),a=0|Object(S.d)(1,e),u=0|Object(S.d)(2,e);return new E(0,[r,a,u])},Object(S.l)(Object(S.i)(0,2)));else{for(var r=0;r<=n-1;r++){var a=D[r],u=a.value;e=new m.b(f(u),e)}e=Object(m.g)(e)}B(e),v(),d();var o=document.getElementsByClassName("color");return Object(S.f)(function(t,n){o[t].style.background=l(n)},B()),N()(),c(B()),null}function h(){var t=Object(m.c)(3,function(t){return 5}),n=function(n){return Object(S.l)(Object(S.n)(t,n))}(B()),e=B().length-1|0,r=Object(m.a)(function(t){return t},Object(m.e)(function(t,n){var r=function(t){var n=o(t);if(2===n.tag)return[n.data[0],n.data[1],n.data[2]];throw new Error("this should never happen")},a=r(n[1]),u=r(t===e?B().head:Object(S.d)(t+1,B())),i=function(t,n){return t-n},c=[i(u[0],a[0]),i(u[1],a[1]),i(u[2],a[2])],f=n[0],l=function(t){var n=t+1;return new C(1,new E(2,[a[0]+c[0]/f*n,a[1]+c[1]/f*n,a[2]+c[2]/f*n]))};return new m.b(new C(0,new E(2,[a[0],a[1],a[2]])),Object(m.c)(n[0],l))},n)),a=document.getElementById("wheel");a.innerHTML="",Object(S.e)(function(t){a.appendChild(t)},function(t){return Object(m.d)(function(t){return s(t)},t)}(r))}e.d(n,"a",function(){return E}),e.d(n,"c",function(){return C}),e.d(n,"b",function(){return x}),e.d(n,"j",function(){return T}),e.d(n,"k",function(){return A}),e.d(n,"l",function(){return I}),n.q=a,n.v=u,n.t=o,n.u=i,n.r=c,e.d(n,"h",function(){return M}),e.d(n,"e",function(){return B}),e.d(n,"g",function(){return D}),e.d(n,"p",function(){return U}),e.d(n,"o",function(){return N}),n.i=f,n.s=l,n.f=s,n.w=d,n.x=v,n.m=b,n.n=h,e.d(n,"d",function(){return F});var g=e(2),y=e(0),p=e(4),m=e(6),w=e(12),O=e(13),j=e(14),S=e(1),k=function(){function t(t,n){for(var e=0;e<n.length;e++){var r=n[e];r.enumerable=r.enumerable||!1,r.configurable=!0,"value"in r&&(r.writable=!0),Object.defineProperty(t,r.key,r)}}return function(n,e,r){return e&&t(n.prototype,e),r&&t(n,r),n}}(),E=function(){function t(n,e){r(this,t),this.tag=n,this.data=e}return k(t,[{key:g.a.reflection,value:function(){return{type:"colorwheel.Color",interfaces:["FSharpUnion","System.IEquatable","System.IComparable"],cases:[["RGB","number","number","number"],["XYZ","number","number","number"],["LAB","number","number","number"]]}}},{key:"Equals",value:function(t){return this===t||this.tag===t.tag&&Object(y.d)(this.data,t.data)}},{key:"CompareTo",value:function(t){return 0|Object(y.b)(this,t)}}]),t}();Object(g.b)("colorwheel.Color",E);var C=function(){function t(n,e){r(this,t),this.tag=n,this.data=e}return k(t,[{key:g.a.reflection,value:function(){return{type:"colorwheel.Results",interfaces:["FSharpUnion","System.IEquatable","System.IComparable"],cases:[["Primary",E],["Filled",E]]}}},{key:"Equals",value:function(t){return this===t||this.tag===t.tag&&Object(y.d)(this.data,t.data)}},{key:"CompareTo",value:function(t){return 0|Object(y.b)(this,t)}}]),t}();Object(g.b)("colorwheel.Results",C);var x=function(){function t(n,e){r(this,t),this.tag=n,this.data=e}return k(t,[{key:g.a.reflection,value:function(){return{type:"colorwheel.RefreshFrom",interfaces:["FSharpUnion","System.IEquatable","System.IComparable"],cases:[["HexBox"],["RGBBox"]]}}},{key:"Equals",value:function(t){return this===t||this.tag===t.tag&&Object(y.d)(this.data,t.data)}},{key:"CompareTo",value:function(t){return 0|Object(y.b)(this,t)}}]),t}();Object(g.b)("colorwheel.RefreshFrom",x);for(var T=95.047,A=100,I=108.883,M=Object(p.b)(Object(p.b)(Object(p.b)(localStorage.getItem("colors"),null,function(t){return function(t){return t}(function(t){return JSON.parse(t)}(t))}),null,function(t){return Object(m.f)(t)}),Object(m.f)([new E(0,[217,64,37]),new E(0,[203,232,143]),new E(0,[183,206,228])])),B=Object(y.c)(M),D=document.getElementsByClassName("color-input"),U=Object(m.f)([document.getElementById("input-r-color-one"),document.getElementById("input-g-color-one"),document.getElementById("input-b-color-one"),document.getElementById("input-r-color-two"),document.getElementById("input-g-color-two"),document.getElementById("input-b-color-two"),document.getElementById("input-r-color-three"),document.getElementById("input-g-color-three"),document.getElementById("input-b-color-three")]),N=Object(y.c)(Object(w.a)(function(){})),q=0;q<=~~D.length-1;q++)!function(t){var n=D[t];n.addEventListener("blur",function(t){return b(new x(0))}),n.addEventListener("submit",function(t){return b(new x(0))}),n.addEventListener("change",function(t){return b(new x(0))}),n.value=l(function(n){return Object(S.d)(t,n)}(B()))}(q);Object(S.f)(function(t,n){var e=i(Object(S.d)(~~(t/3),B())),r=void 0;if(0!==e.tag)throw new Error("should never happen");r=[e.data[0],e.data[1],e.data[2]];var a=t%3|0;if(0===a)n.value=r[0].toString();else if(1===a)n.value=r[1].toString();else{if(2!==a)throw new Error("/Users/kdummann/source/colorwheel/src/App.fs",387,10);n.value=r[2].toString()}n.addEventListener("blur",function(t){return b(new x(1))}),n.addEventListener("submit",function(t){return b(new x(1))}),n.addEventListener("change",function(t){return b(new x(1))})},U);var F=document.getElementsByClassName("color");Object(S.f)(function(t,n){F[t].style.background=l(n)},B()),N(function(){h()}),h()},function(t,n,e){"use strict";function r(t,n){if(!(t instanceof n))throw new TypeError("Cannot call a class as a function")}function a(t,n){for(var e=[],r=n[Symbol.iterator](),a=k(),u=r.next();!u.done;){var o=t(u.value),i=C(o,a);null==i?(e.push(o),a=E(o,[u.value],a)):Object(A.c)(i).push(u.value),u=r.next()}return e.map(function(t){return[t,a.get(t)]})}function u(t,n){for(;;){if(1===n.tag)return t+1|0;{if(2!==n.tag)return 0|t;t=u(t+1,n.data[2]),n=n.data[3]}}}function o(t){return u(0,t)}function i(){return new U(0)}function c(t){return 1===t.tag?1:2===t.tag?t.data[4]:0}function f(t,n,e,r){switch(0===t.tag&&0===r.tag?0:1){case 0:return new U(1,[n,e]);case 1:var a=0|c(t),u=0|c(r);return new U(2,[n,e,t,r,1+(0|(a<u?u:a))])}throw new Error("internal error: Map.tree_mk")}function l(t,n,e,r){var a=c(t),u=c(r);if(u>a+2){if(2===r.tag){if(c(r.data[2])>a+1){if(2===r.data[2].tag)return f(f(t,n,e,r.data[2].data[2]),r.data[2].data[0],r.data[2].data[1],f(r.data[2].data[3],r.data[0],r.data[1],r.data[3]));throw new Error("rebalance")}return f(f(t,n,e,r.data[2]),r.data[0],r.data[1],r.data[3])}throw new Error("rebalance")}if(a>u+2){if(2===t.tag){if(c(t.data[3])>u+1){if(2===t.data[3].tag)return f(f(t.data[2],t.data[0],t.data[1],t.data[3].data[2]),t.data[3].data[0],t.data[3].data[1],f(t.data[3].data[3],n,e,r));throw new Error("rebalance")}return f(t.data[2],t.data[0],t.data[1],f(t.data[3],n,e,r))}throw new Error("rebalance")}return f(t,n,e,r)}function s(t,n,e,r){if(1===r.tag){var a=t.Compare(n,r.data[0]);return a<0?new U(2,[n,e,new U(0),r,2]):0===a?new U(1,[n,e]):new U(2,[n,e,r,new U(0),2])}if(2===r.tag){var u=t.Compare(n,r.data[0]);return u<0?l(s(t,n,e,r.data[2]),r.data[0],r.data[1],r.data[3]):0===u?new U(2,[n,e,r.data[2],r.data[3],r.data[4]]):l(r.data[2],r.data[0],r.data[1],s(t,n,e,r.data[3]))}return new U(1,[n,e])}function d(t,n,e){var r=v(t,n,e);if(null==r)throw new Error("key not found: "+n);return Object(A.c)(r)}function v(t,n,e){t:for(;;){if(1===e.tag){var r=0|t.Compare(n,e.data[0]);return 0===r?new A.a(e.data[1]):null}{if(2!==e.tag)return null;var a=0|t.Compare(n,e.data[0]);if(a<0){t=t,n=n,e=e.data[2];continue t}if(0===a)return new A.a(e.data[1]);t=t,n=n,e=e.data[3]}}}function b(t){if(1===t.tag)return[t.data[0],t.data[1],new U(0)];if(2===t.tag){if(0===t.data[2].tag)return[t.data[0],t.data[1],t.data[3]];var n=b(t.data[2]);return[n[0],n[1],f(n[2],t.data[0],t.data[1],t.data[3])]}throw new Error("internal error: Map.spliceOutSuccessor")}function h(t,n,e){if(1===e.tag){return 0===t.Compare(n,e.data[0])?new U(0):e}if(2===e.tag){var r=t.Compare(n,e.data[0]);if(r<0)return l(h(t,n,e.data[2]),e.data[0],e.data[1],e.data[3]);if(0===r){if(0===e.data[2].tag)return e.data[3];if(0===e.data[3].tag)return e.data[2];var a=b(e.data[3]);return f(e.data[2],a[0],a[1],a[2])}return l(e.data[2],e.data[0],e.data[1],h(t,n,e.data[3]))}return i()}function g(t,n,e){t:for(;;){if(1===e.tag)return 0===t.Compare(n,e.data[0]);{if(2!==e.tag)return!1;var r=0|t.Compare(n,e.data[0]);if(r<0){t=t,n=n,e=e.data[2];continue t}if(0===r)return!0;t=t,n=n,e=e.data[3]}}}function y(t,n){return 1===n.tag?new U(1,[n.data[0],t(n.data[0],n.data[1])]):2===n.tag?new U(2,[n.data[0],t(n.data[0],n.data[1]),y(t,n.data[2]),y(t,n.data[3]),n.data[4]]):i()}function p(t,n,e){for(var r=e.next();!r.done;)n=s(t,r.value[0],r.value[1],n),r=e.next();return n}function m(t,n){var e=n[Symbol.iterator]();return p(t,i(),e)}function w(t){return null!=t.tail?1===t.head.tag?t:w(2===t.head.tag?Object(T.b)([t.head.data[2],new U(1,[t.head.data[0],t.head.data[1]]),t.head.data[3]],t.tail):t.tail):new T.a}function O(t){return{stack:w(new T.a(t,new T.a)),started:!1}}function j(t){function n(t){if(null==t.stack.tail)return null;if(1===t.stack.head.tag)return[t.stack.head.data[0],t.stack.head.data[1]];throw new Error("Please report error: Map iterator, unexpected stack for current")}if(t.started){if(null==t.stack.tail)return{done:!0,value:null};if(1===t.stack.head.tag)return t.stack=w(t.stack.tail),{done:null==t.stack.tail,value:n(t)};throw new Error("Please report error: Map iterator, unexpected stack for moveNext")}return t.started=!0,{done:null==t.stack.tail,value:n(t)}}function S(t,n){var e=new N;return e.tree=n,e.comparer=t||new x.a,e}function k(t,n){return n=n||new x.a,S(n,t?m(n,t):i())}function E(t,n,e){return S(e.comparer,s(e.comparer,t,n,e.tree))}function C(t,n){return v(n.comparer,t,n.tree)}n.a=a;var x=e(11),T=e(3),A=e(4),I=e(1),M=e(2),B=e(0),D=function(){function t(t,n){for(var e=0;e<n.length;e++){var r=n[e];r.enumerable=r.enumerable||!1,r.configurable=!0,"value"in r&&(r.writable=!0),Object.defineProperty(t,r.key,r)}}return function(n,e,r){return e&&t(n.prototype,e),r&&t(n,r),n}}(),U=function t(n,e){r(this,t),this.tag=0|n,this.data=e},N=function(){function t(){r(this,t)}return D(t,[{key:"ToString",value:function(){return"map ["+Array.from(this).map(function(t){return Object(B.f)(t)}).join("; ")+"]"}},{key:"Equals",value:function(t){return 0===this.CompareTo(t)}},{key:"CompareTo",value:function(t){var n=this;return this===t?0:Object(I.a)(function(t,e){var r=n.comparer.Compare(t[0],e[0]);return 0!==r?r:Object(B.a)(t[1],e[1])},this,t)}},{key:Symbol.iterator,value:function(){var t=O(this.tree);return{next:function(){return j(t)}}}},{key:"entries",value:function(){return this[Symbol.iterator]()}},{key:"keys",value:function(){return Object(I.g)(function(t){return t[0]},this)}},{key:"values",value:function(){return Object(I.g)(function(t){return t[1]},this)}},{key:"get",value:function(t){return d(this.comparer,t,this.tree)}},{key:"has",value:function(t){return g(this.comparer,t,this.tree)}},{key:"set",value:function(t,n){this.tree=s(this.comparer,t,n,this.tree)}},{key:"delete",value:function(t){var n=o(this.tree);return this.tree=h(this.comparer,t,this.tree),n>o(this.tree)}},{key:"clear",value:function(){this.tree=i()}},{key:M.a.reflection,value:function(){return{type:"Microsoft.FSharp.Collections.FSharpMap",interfaces:["System.IEquatable","System.IComparable","System.Collections.Generic.IDictionary"]}}},{key:"size",get:function(){return o(this.tree)}}]),t}()},function(t,n,e){"use strict";function r(t,n){if(!(t instanceof n))throw new TypeError("Cannot call a class as a function")}var a=e(2),u=e(0),o=function(){function t(t,n){for(var e=0;e<n.length;e++){var r=n[e];r.enumerable=r.enumerable||!1,r.configurable=!0,"value"in r&&(r.writable=!0),Object.defineProperty(t,r.key,r)}}return function(n,e,r){return e&&t(n.prototype,e),r&&t(n,r),n}}(),i=function(){function t(n){r(this,t),this.Compare=n||u.a}return o(t,[{key:a.a.reflection,value:function(){return{interfaces:["System.IComparer"]}}}]),t}();n.a=i},function(t,n,e){"use strict";function r(t){if(Array.isArray(t)){for(var n=0,e=Array(t.length);n<t.length;n++)e[n]=t[n];return e}return Array.from(t)}function a(t,n){if(!0===t.curried)return t;var e=function(){for(var e=arguments.length,u=Array(e),o=0;o<e;o++)u[o]=arguments[o];var i=Math.max(u.length,1);if(n=Math.max(n||t.length,1),i>=n){var c=u.splice(n),f=t.apply(void 0,u);if("function"==typeof f){var l=a(f);return 0===c.length?l:l.apply(void 0,r(c))}return f}return a(function(){for(var n=arguments.length,e=Array(n),a=0;a<n;a++)e[a]=arguments[a];return t.apply(void 0,r(u.concat(e)))},n-i)};return e.curried=!0,e}n.a=a},function(t,n,e){"use strict";function r(t,n){var e=i.exec(t);if(null!=e){if(null==n)switch(e[2]){case"0b":n=2;break;case"0o":n=8;break;case"0x":n=16;break;default:n=10}switch(n){case 2:return c.test(e[3])?null:[e,2];case 8:return f.test(e[3])?null:[e,8];case 10:return l.test(e[3])?null:[e,10];case 16:return[e,16];default:throw new Error("Invalid Base.")}}return null}function a(t,n,e){var a=r(t,n);if(null!==a){var u=o(a,2),i=o(u[0],4),c=i[1],f=i[3],l=u[1],s=parseInt((c||"")+f,l);if(!Number.isNaN(s))return[!0,s]}return[!1,e]}function u(t,n){var e=a(t,n,0);if(e[0])return e[1];throw new Error("Input string was not in a correct format.")}n.a=u;var o=function(){function t(t,n){var e=[],r=!0,a=!1,u=void 0;try{for(var o,i=t[Symbol.iterator]();!(r=(o=i.next()).done)&&(e.push(o.value),!n||e.length!==n);r=!0);}catch(t){a=!0,u=t}finally{try{!r&&i.return&&i.return()}finally{if(a)throw u}}return e}return function(n,e){if(Array.isArray(n))return n;if(Symbol.iterator in Object(n))return t(n,e);throw new TypeError("Invalid attempt to destructure non-iterable instance")}}(),i=/^\s*([\+\-])?(0[xob])?([0-9a-fA-F]+)\s*$/,c=/[^01]/,f=/[^0-7]/,l=/[^0-9]/},function(t,n,e){"use strict";function r(t){return t<0?"ff"+(16777215-(Math.abs(t)-1)).toString(16):t.toString(16)}function a(t){return{input:t,cont:f(t)}}function u(t){return t.cont(function(t){console.log(t)})}function o(t){return t.cont(function(t){return t})}function i(t,n){return t.replace(v,function(t,e,a,u,o,i){switch(i){case"f":case"F":n=n.toFixed(o||6);break;case"g":case"G":n=n.toPrecision(o);break;case"e":case"E":n=n.toExponential(o);break;case"O":n=Object(d.f)(n);break;case"A":n=Object(d.f)(n,!0);break;case"x":n=r(Number(n));break;case"X":n=r(Number(n)).toUpperCase()}var c=a.indexOf("+")>=0&&parseInt(n,10)>=0;if(u=parseInt(u,10),!isNaN(u)){var f=u>=0&&a.indexOf("0")>=0?"0":" ";n=l(n,Math.abs(u)-(c?1:0),f,u<0)}return(e+(c?"+"+n:n)).replace(/%/g,"%%")})}function c(t,n){var e=function(){for(var e=arguments.length,r=Array(e),a=0;a<e;a++)r[a]=arguments[a];var u=t,o=!0,f=!1,l=void 0;try{for(var s,d=r[Symbol.iterator]();!(o=(s=d.next()).done);o=!0){u=i(u,s.value)}}catch(t){f=!0,l=t}finally{try{!o&&d.return&&d.return()}finally{if(f)throw l}}return v.test(u)?c(u,n):n(u.replace(/%%/g,"%"))};return e.curried=!0,e}function f(t){return function(n){return v.test(t)?c(t,n):n(t)}}function l(t,n,e,r){e=e||" ",t=String(t),n-=t.length;for(var a=0;a<n;a++)t=r?t+e:e+t;return t}n.a=a,n.b=u,n.c=o;var s=e(5),d=(e(15),e(0)),v=/(^|[^%])%([0+ ]*)(-?\d+)?(?:\.(\d+))?(\w)/,b=/\{(\d+)(,-?\d+)?(?:\:(.+?))?\}/g},function(t,n,e){"use strict";function r(t){return t.replace(/[\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|]/g,"\\$&")}n.a=r}]);
//# sourceMappingURL=bundle.js.map