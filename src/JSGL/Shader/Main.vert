#version 330 core
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;

uniform mat4 transform;
out vec3 vNormal;
out vec4 vColor;

void main(){
    vNormal = normal;
    vColor = vec4(0.5*position+0.5, 1.0);
    gl_Position = transform * vec4(position, 1.0);
}

