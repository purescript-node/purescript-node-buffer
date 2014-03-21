module.exports = function(grunt) {

    "use strict";

    grunt.initConfig({ 
    
        clean: ["externs", "js"],
        
        libFiles: [
            "src/**/*.purs", 
            "bower_components/purescript-*/src/**/*.purs",
            "bower_components/purescript-*/src/**/*.purs.hs"
        ],
    
        "purescript-make": {
            options: {
                tco: true,
                magicDo: true
            },
            lib: {
                src: "<%=libFiles%>"
            }
        }
        
    });

    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-contrib-clean");
    
    grunt.registerTask("default", ["purescript-make:lib"]);
};
