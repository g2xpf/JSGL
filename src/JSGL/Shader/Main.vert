#version 330 core
layout (location = 0) in vec2 position;
layout (location = 1) in vec3 color;
out vec4 vColor;

void main(){
    vColor = vec4(color, 1.0);
    gl_Position = vec4(position * 0.8, 0.0, 1.0);
}

